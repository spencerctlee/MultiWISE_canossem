# ------------------------------------------------------------
# MutliWiSE Dashboard
# Updated 22 October 2025
# ------------------------------------------------------------

options(shiny.maxRequestSize = 5000e6)  # NOTE: Using 5 GB upload cap for dashboard to run locally
options(stringsAsFactors = FALSE)
suppressPackageStartupMessages({
  # UI deps
  library(shiny)
  library(bslib)
  library(readr)
  library(DT)
  library(dplyr)
  library(tibble)
  library(lubridate)
  library(ggplot2)
  library(shinyjs)
  # Backend deps
  library(data.table)
  library(tidyr)
  library(stringi)
  library(purrr)
  library(ggtext)
  library(gridExtra)
  library(patchwork)
  library(gt)
  library(readxl)
  library(httr)
})

`%||%` <- function(x, y) if (is.null(x)) y else x  # Tiny helper: return y when x is NULL

#### Backend ####
httr::set_config(httr::timeout(960))  # Relatively long timeout in case any remote I/O happens

# --- Output directory root (used when bundling results) ---
out_dir_default <- getOption(
  "multiwise.out_dir",
  Sys.getenv("MW_OUT_DIR", file.path(tempdir(), "multiwise_runs"))
)
dir.create(out_dir_default, recursive = TRUE, showWarnings = FALSE)

# ---- Default threshold for Metric 6 (overridable via UI) ----
total_pm25_threshold_default <- 25  # Users can change this in the UI

# ---- Upload cap used by server-side checks ----
# MAX_UPLOAD_BYTES <- 250e6 # NOTE: No upload cap for running dashboard locally

# ---- Helpers ------------------------------------------------

check_date_format <- function(date_vec) {
  # Validate strict YYYY-MM-DD; return count of invalid rows
  x <- trimws(as.character(date_vec))
  invalid_blank <- is.na(x) | x == ""
  matches_ymd   <- grepl("^\\d{4}-\\d{2}-\\d{2}$", x)
  to_check      <- which(!invalid_blank & matches_ymd)
  parsed        <- as.Date(x[to_check], format = "%Y-%m-%d")
  roundtrip_ok  <- !is.na(parsed) & (format(parsed, "%Y-%m-%d") == x[to_check])
  invalid <- invalid_blank | !matches_ymd
  invalid[to_check] <- !roundtrip_ok
  sum(invalid, na.rm = TRUE)
}

read_and_validate_daily_file <- function(path,
                                         required_cols = c("ID", "Date", "Total_PM25")) {
  # Read CSV/XLS/XLSX and ensure required columns + valid dates
  ext <- tolower(tools::file_ext(path))
  if (!ext %in% c("csv", "xls", "xlsx"))
    stop(sprintf("Error: %s has unsupported extension .%s. Acceptable formats are .csv, .xls, .xlsx.",
                 basename(path), ext), call. = FALSE)
  
  df <- switch(ext,
               csv  = data.table::fread(path, showProgress = FALSE),
               xls  = readxl::read_excel(path),
               xlsx = readxl::read_excel(path))
  
  if (!all(required_cols %in% names(df))) {
    stop("Error: Unable to process data. The provided file does not have the correct column names. Please use the following column names: ID, Date, Total_PM25.",
         call. = FALSE)
  }
  
  bad_dates <- check_date_format(df$Date)
  if (bad_dates > 0) {
    stop(sprintf("Error: Unable to process data. The Date column in the provided file has %d date(s) that are incorrectly formatted or missing. Please ensure all dates are formatted as follows: YYYY-MM-DD.",
                 bad_dates), call. = FALSE)
  }
  
  # Standardize types
  dt <- as.data.table(df)[, .(
    ID         = as.character(ID),
    Date       = as.Date(as.character(Date)),
    Total_PM25 = suppressWarnings(as.numeric(Total_PM25))
  )]
  dt[]
}

# ---- Daily -> weekly aggregation (with QA guardrails) -----------------------
fn_build_weekly_from_daily <- function(dt_daily,
                                       min_days_per_window = 0.75,
                                       remove_edge_incomplete = TRUE) {
  id             <- unique(dt_daily$ID)
  exposure_start <- min(dt_daily$Date)
  exposure_end   <- max(dt_daily$Date)
  
  # Fill missing dates to ensure contiguous daily coverage
  full_dates <- data.table(Date = seq(exposure_start, exposure_end, by = "day"))
  dt_daily   <- merge(full_dates, dt_daily, by = "Date", all.x = TRUE)
  dt_daily[, ID := id]
  
  # Basic length/coverage checks
  n_expected  <- as.numeric(exposure_end - exposure_start) + 1L
  n_available <- sum(!is.na(dt_daily$Total_PM25))
  coverage    <- n_available / n_expected
  
  if (n_expected < 365) {
    stop("Error: Unable to process ID. The provided time series is less than 1 year in length. Please ensure the time series is at least 1 year in length.",
         call. = FALSE)
  }
  if (coverage < min_days_per_window) {
    stop(sprintf("Error: Unable to process ID. The provided file only provides PM2.5 data for %.1f%% of the days in the time series. ≥75%% data coverage and at least 1 year of data is required for processing.",
                 100 * coverage), call. = FALSE)
  }
  if (n_available < 365) {
    miss_pct <- 100 * (1 - n_available / n_expected)
    stop(sprintf("Error: Unable to process ID. The ID had PM2.5 data missing for %.1f%% of days in the time series, leading to less than a year of data available for processing. At least 1 year of data is required for processing.",
                 miss_pct), call. = FALSE)
  }
  
  # Compute epi weeks and continuous week indices
  dt_daily <- dt_daily[order(Date)]
  dt_daily[, `:=`(
    epi_week = lubridate::epiweek(Date),
    epi_year = lubridate::epiyear(Date)
  )]
  dt_daily[, week_key  := paste0(epi_year, "-", sprintf("%02d", epi_week))]
  dt_daily[, cont_week := data.table::rleid(week_key)]
  
  # Weekly aggregation
  weekly <- dt_daily[, .(
    weekly_pm25_sum_i  = sum(Total_PM25, na.rm = TRUE),
    n_days             = sum(!is.na(Total_PM25)),
    epiweek_start_date = min(Date),
    epiweek_end_date   = max(Date)
  ), by = cont_week][order(epiweek_start_date)]
  
  # Trim partial first/last weeks if requested
  if (remove_edge_incomplete) {
    while (nrow(weekly) > 0 && weekly$n_days[1L] < 7)           weekly <- weekly[-1L]
    while (nrow(weekly) > 0 && weekly$n_days[nrow(weekly)] < 7) weekly <- weekly[-nrow(weekly)]
  }
  if (nrow(weekly) == 0)
    stop(sprintf("Error: Unable to process ID. No overlapping data found for ID %s.", id),
         call. = FALSE)
  
  weekly[, weekly_pm25_avg_i := weekly_pm25_sum_i / pmax(n_days, 1)]
  weekly[, week_sequence      := seq_len(.N)]
  weekly[, ID                 := id]
  
  # If there were missing days, emit a helpful warning
  if (n_available < n_expected) {
    missing_pct    <- 100 * (1 - coverage)
    weeks_with_zero <- sum(weekly$n_days == 0, na.rm = TRUE)
    if (weeks_with_zero > 0) {
      warning(sprintf("Warning: The ID had PM2.5 data missing for %.1f%% of the days in the time series, and %d weeks had no data. For weeks with no data, total and non-WFS PM2.5 were set to the counterfactual value and WFS PM2.5 was set to 0. Weeks with missing data can be identified using the 'n_days' column in the 'Weekly_PM25_Estimates.csv' file.",
                      missing_pct, weeks_with_zero), call. = FALSE)
    } else {
      warning(sprintf("Warning: The ID had PM2.5 data missing for %.1f%% of the days in the time series. ID was processed using available data. Weeks with missing data can be identified using the 'n_days' column in the Weekly_PM25_Estimates.csv file.",
                      missing_pct), call. = FALSE)
    }
  }
  weekly[]
}

# ---- Z & counterfactual trajectory -----------------------------------------
fn_calc_modified_z <- function(x) {
  # MAD-based robust Z-score
  med  <- median(x, na.rm = TRUE)
  madv <- mad(x, constant = 1.4826, na.rm = TRUE)
  if (madv == 0) return(rep(NA_real_, length(x)))
  (x - med) / madv
}

fn_build_counterfactual_trajectory <- function(df) {
  # Build per-week totals, counterfactual (non-WFS) trajectory, and cumulative sums
  df <- data.table::copy(df)[order(week_sequence)]
  df[, start_date := epiweek_start_date]
  df[, modified_z := fn_calc_modified_z(weekly_pm25_sum_i)]
  df[, wildfire_season := (lubridate::month(epiweek_end_date)   >= 5 &
                             lubridate::month(epiweek_start_date) <= 10)]
  
  # Counterfactual slope = median of stable (|Z|<=2) complete weeks
  counterfactual_slope <- df[
    !is.na(modified_z) & modified_z >= -2 & modified_z <= 2 & n_days == 7,
    median(weekly_pm25_sum_i, na.rm = TRUE)
  ]
  if (!is.finite(counterfactual_slope)) counterfactual_slope <- 0
  
  # Replace zero-day weeks with counterfactual slope
  idx_zero <- which(df$n_days == 0)
  if (length(idx_zero)) df$weekly_pm25_sum_i[idx_zero] <- counterfactual_slope
  
  # Cumulative totals
  df[, cumulative_total_pm25_sum_individual := cumsum(weekly_pm25_sum_i)]
  
  # Non-WFS: use counterfactual slope during high-Z weeks in wildfire season; otherwise observed
  df[, weekly_non_wfs_pm25_sum_i := ifelse(
    dplyr::coalesce(modified_z > 2, FALSE) & dplyr::coalesce(wildfire_season, FALSE),
    counterfactual_slope, weekly_pm25_sum_i)]
  if (length(idx_zero)) df$weekly_non_wfs_pm25_sum_i[idx_zero] <- counterfactual_slope
  
  # WFS component = observed - non-WFS
  df[, weekly_wfs_pm25_sum_i := weekly_pm25_sum_i - weekly_non_wfs_pm25_sum_i]
  if (length(idx_zero)) df$weekly_wfs_pm25_sum_i[idx_zero] <- 0
  
  # Convert sums to means (divide by days; zero-day weeks treated as 7)
  df[, weekly_pm25_avg_i         := ifelse(n_days == 0, weekly_pm25_sum_i         / 7, weekly_pm25_sum_i         / pmax(n_days, 1))]
  df[, weekly_non_wfs_pm25_avg_i := ifelse(n_days == 0, weekly_non_wfs_pm25_sum_i / 7, weekly_non_wfs_pm25_sum_i / pmax(n_days, 1))]
  df[, weekly_wfs_pm25_avg_i     := ifelse(n_days == 0, weekly_wfs_pm25_sum_i     / 7, weekly_wfs_pm25_sum_i     / pmax(n_days, 1))]
  
  # Running totals for components
  df[, cumulative_non_wfs_pm25_sum_individual := cumsum(weekly_non_wfs_pm25_sum_i)]
  df[, cumulative_wfs_pm25_sum_individual     := cumsum(weekly_wfs_pm25_sum_i)]
  df[]
}

# ---- Episode labelling ------------------------------------------------------
fn_identify_smoke_episodes <- function(is_smoke_impacted, cont_week,
                                       weekly_sum_wfs_pm25_values,
                                       episode_threshold = 0,
                                       num_weeks_in_episode = 2,
                                       max_no_smoke_gap = 3,
                                       weeks_needed_above_threshold = 1) {
  # Convert weekly flags to episode IDs with simple bridging rules
  n <- length(is_smoke_impacted); epi_id <- integer(n)
  cur_id <- 0; in_epi <- FALSE; smoke_wks <- wks_above <- gap <- 0
  bridge <- integer(0); fail <- function(id) epi_id[epi_id == id] <<- 0
  
  for (i in seq_len(n)) {
    # If there's a break in cont_week, close out any pending episode
    if (i > 1 && cont_week[i] - cont_week[i-1] > 1) {
      if (in_epi && (smoke_wks < num_weeks_in_episode ||
                     wks_above < weeks_needed_above_threshold)) fail(cur_id)
      in_epi <- FALSE; smoke_wks <- wks_above <- gap <- 0; bridge <- integer(0)
    }
    if (is_smoke_impacted[i] == 1) {
      # Start or continue an episode
      if (!in_epi) { cur_id <- cur_id + 1; in_epi <- TRUE; smoke_wks <- wks_above <- 0 }
      if (length(bridge) > 0) { epi_id[bridge] <- cur_id; bridge <- integer(0) }
      smoke_wks <- smoke_wks + 1
      val <- weekly_sum_wfs_pm25_values[i]
      if (!is.na(val) && val > episode_threshold) wks_above <- wks_above + 1
      epi_id[i] <- cur_id; gap <- 0
    } else if (in_epi) {
      # Track small gaps; bridge them if within limit
      gap <- gap + 1; bridge <- c(bridge, i)
      if (gap > max_no_smoke_gap) {
        if (smoke_wks < num_weeks_in_episode ||
            wks_above < weeks_needed_above_threshold) fail(cur_id)
        in_epi <- FALSE; bridge <- integer(0)
      }
    }
  }
  # Finalize the last episode if needed
  if (in_epi && (smoke_wks < num_weeks_in_episode ||
                 wks_above < weeks_needed_above_threshold)) fail(cur_id)
  # Trim trailing non-smoke weeks from each episode
  for (id in setdiff(unique(epi_id), 0)) {
    idx       <- which(epi_id == id)
    smoke_idx <- idx[is_smoke_impacted[idx] == 1]
    epi_id[idx[idx > max(smoke_idx)]] <- 0
  }
  # Renumber to 1..K
  uniq <- setdiff(unique(epi_id), 0)
  for (k in seq_along(uniq)) epi_id[epi_id == uniq[k]] <- k
  epi_id
}

# ---- Metrics ---------------------------------------------------------------
fn_compute_metrics <- function(df, exposure_start, exposure_end,
                               total_pm25_threshold = total_pm25_threshold_default) {
  # Compute the 12 MultiWiSE metrics + a few extras
  MICRO_TO_MILLI <- 1 / 1000
  df <- df %>% dplyr::mutate(is_smoke_impacted = as.integer(weekly_wfs_pm25_avg_i > 0))
  
  total_smoke_weeks <- sum(df$is_smoke_impacted, na.rm = TRUE)
  mean_smoke_week   <- if (total_smoke_weeks > 0)
    mean(df$weekly_wfs_pm25_avg_i[df$is_smoke_impacted == 1], na.rm = TRUE) else 0
  
  weeks_wfs_over_5                <- sum(df$weekly_wfs_pm25_avg_i > 5,  na.rm = TRUE)
  weeks_total_pm25_over_threshold <- sum(df$weekly_wfs_pm25_avg_i > 0 & df$weekly_pm25_avg_i > total_pm25_threshold, na.rm = TRUE)
  
  num_normal_episodes <- max(df$episode_id,        na.rm = TRUE)
  num_severe_episodes <- max(df$severe_episode_id, na.rm = TRUE)
  
  # Longest episode length in weeks (span, not just weeks with smoke)
  epi_summary_span <- df %>%
    dplyr::filter(episode_id > 0) %>%
    dplyr::group_by(episode_id) %>%
    dplyr::summarise(active_weeks = n(), .groups = "drop")
  longest_episode_len <- if (nrow(epi_summary_span) > 0)
    max(epi_summary_span$active_weeks, na.rm = TRUE) else 0
  
  # Worst (highest mean) WFS exposure across episodes
  epi_df <- df %>% dplyr::filter(episode_id > 0, is_smoke_impacted == 1)
  episode_summary <- epi_df %>%
    dplyr::group_by(episode_id) %>%
    dplyr::summarise(avg_pm25 = mean(weekly_wfs_pm25_avg_i, na.rm = TRUE), .groups = "drop")
  worst_episode_exposure <- if (nrow(episode_summary) > 0)
    max(episode_summary$avg_pm25, na.rm = TRUE) else 0
  
  # Cumulative WFS during severe episodes
  severe_pm25_cum <- df %>% dplyr::filter(severe_episode_id > 0, is_smoke_impacted == 1) %>%
    dplyr::summarise(sum(weekly_wfs_pm25_sum_i, na.rm = TRUE)) %>% dplyr::pull()
  
  # Convert sums to mg/m^3
  obs_cumul_mg <- dplyr::last(df$cumulative_total_pm25_sum_individual) * MICRO_TO_MILLI
  wfs_cumul_mg <- dplyr::last(df$cumulative_wfs_pm25_sum_individual)   * MICRO_TO_MILLI
  cf_cumul_mg  <- sum(df$weekly_non_wfs_pm25_sum_i, na.rm = TRUE) * MICRO_TO_MILLI
  
  pct_wfs_from_severe <- if (wfs_cumul_mg > 0)
    100 * severe_pm25_cum * MICRO_TO_MILLI / wfs_cumul_mg else NA_real_
  
  # Average time between episodes (weeks)
  if (nrow(epi_df) == 0) {
    avg_time_between <- round(as.numeric(exposure_end - exposure_start) / 7, 2)
  } else {
    episode_info <- epi_df %>%
      dplyr::group_by(episode_id) %>%
      dplyr::summarise(start_epi_date = min(epiweek_start_date, na.rm = TRUE),
                       end_epi_date   = max(epiweek_end_date,   na.rm = TRUE),
                       .groups        = "drop") %>%
      dplyr::arrange(start_epi_date)
    gap_starts <- c(exposure_start, episode_info$end_epi_date)
    gap_ends   <- c(episode_info$start_epi_date, exposure_end)
    gaps_days  <- as.numeric(gap_ends - gap_starts)
    avg_time_between <- round(mean(gaps_days) / 7, 2)
  }
  
  # Assemble metrics 1–12 with readable names
  name6 <- sprintf("6_Total_PM25_exceeds_%s", as.character(total_pm25_threshold))
  numbered_list <- list(
    `1_Cumulative_WFS_PM25`       = round(wfs_cumul_mg, 2),
    `2_WFS_Fraction`              = round(100 * wfs_cumul_mg / obs_cumul_mg, 2),
    `3_Average_WFS_PM25`          = round(mean_smoke_week, 2),
    `4_Any_WFS`                   = total_smoke_weeks,
    `5_WFS_PM25_exceeds_5`        = weeks_wfs_over_5
  )
  numbered_list[[name6]] <- weeks_total_pm25_over_threshold
  numbered_list <- c(numbered_list, list(
    `7_WFS_Episodes`              = num_normal_episodes,
    `8_Severe_Episodes`           = num_severe_episodes,
    `9_Longest_Episode`           = longest_episode_len,
    `10_Worst_Episode`            = round(worst_episode_exposure, 2),
    `11_WFS_from_Severe_Episodes` = round(pct_wfs_from_severe, 1),
    `12_Average_Recovery`         = avg_time_between
  ))
  numbered <- tibble::as_tibble(numbered_list)
  
  # Extras to help interpret the counterfactual
  cf_vals <- unique(df$weekly_non_wfs_pm25_avg_i[df$is_smoke_impacted == 1])
  cf_vals <- cf_vals[is.finite(cf_vals)]
  cf_scalar <- if (length(cf_vals)) cf_vals[1] else NA_real_
  
  extras <- tibble::tibble(
    `Cumulative_Total_PM25`     = round(obs_cumul_mg, 2),
    `Average_Total_PM25`        = round(mean(df$weekly_pm25_avg_i, na.rm = TRUE), 2),
    `Cumulative_NonWFS_PM25`    = round(cf_cumul_mg, 2),
    `Average_NonWFS_PM25`       = round(mean(df$weekly_non_wfs_pm25_avg_i, na.rm = TRUE), 2),
    `Counterfactual_Value`      = round(cf_scalar, 2)
  )
  dplyr::bind_cols(numbered, extras)
}

# ---- Plotting ----------------------------------------
median_range_color        <- "#B0C6DF"
wildfire_attributable_col <- "#FF8572"

.fn_plot_theme_small <- function() {
  ggplot2::theme_classic(base_size = 16) +
    ggplot2::theme(
      legend.position = "top",
      axis.line  = ggplot2::element_line(linewidth = 0.4),
      axis.ticks = ggplot2::element_line(linewidth = 0.4)
    )
}

fn_plot_cumulative_sum_by_year <- function(df, y_limits = NULL) {
  # Cumulative totals (mg/m^3) split into total / non-WFS / WFS
  df_plot <- df %>% dplyr::mutate(
    total   = cumulative_total_pm25_sum_individual / 1000,
    non_wfs = cumulative_non_wfs_pm25_sum_individual / 1000,
    wfs     = cumulative_wfs_pm25_sum_individual     / 1000
  )
  y_max <- if (is.null(y_limits))
    max(c(df_plot$total, df_plot$non_wfs, df_plot$wfs), na.rm = TRUE) * 1.05 else y_limits[2]
  ggplot2::ggplot(df_plot, ggplot2::aes(x = start_date)) +
    ggplot2::geom_line(ggplot2::aes(y = total,   color = "Total"),   linewidth = 0.9) +
    ggplot2::geom_line(ggplot2::aes(y = non_wfs, color = "Non-WFS"), linewidth = 0.9) +
    ggplot2::geom_line(ggplot2::aes(y = wfs,     color = "WFS"),     linewidth = 0.9) +
    ggplot2::scale_color_manual(values = c("Total"="black","Non-WFS"=median_range_color,"WFS"=wildfire_attributable_col),
                                breaks = c("Total","Non-WFS","WFS"), name = NULL) +
    ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0,0)) +
    ggplot2::scale_y_continuous(expand = c(0,0), limits = c(-0.25, y_max)) +
    ggplot2::labs(x = "Year",
                  y = expression("Cumulative PM"[2.5]*" (mg/m"^3*")")) +
    .fn_plot_theme_small()
}

fn_plot_time_series_weekly_mean <- function(df, y_limits = NULL,
                                            alpha_norm = 0.15,
                                            alpha_severe = 0.20,
                                            fill_norm = "orange",
                                            fill_severe = "red3") {
  # Weekly means (µg/m^3) with episode shading
  df_plot <- df %>% dplyr::mutate(
    wfs     = dplyr::coalesce(weekly_wfs_pm25_avg_i,     0),
    non_wfs = dplyr::coalesce(weekly_non_wfs_pm25_avg_i, 0),
    total   = dplyr::coalesce(weekly_pm25_avg_i,         0)
  )

  y_max <- if (is.null(y_limits))
    max(c(df_plot$total, df_plot$non_wfs, df_plot$wfs), na.rm = TRUE) * 1.05 else y_limits[2]
  plot <- ggplot2::ggplot() 
  
  if(sum(df_plot$episode_id > 0)) {
    epi_spans <- df_plot %>% dplyr::filter(episode_id > 0) %>% dplyr::group_by(episode_id) %>%
      dplyr::summarise(start = min(start_date, na.rm = TRUE),
                       end   = max(epiweek_end_date, na.rm = TRUE), .groups = "drop")
    
    plot <- plot + ggplot2::geom_rect(data = epi_spans, inherit.aes = FALSE,
                                      ggplot2::aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf,
                                                   fill = "Episode"), alpha = alpha_norm) 
  }
  if(sum(df_plot$severe_episode_id > 0)) {
    sev_spans <- df_plot %>% dplyr::filter(severe_episode_id > 0) %>% dplyr::group_by(severe_episode_id) %>%
      dplyr::summarise(start = min(start_date, na.rm = TRUE),
                       end   = max(epiweek_end_date, na.rm = TRUE), .groups = "drop")
    
    plot <- plot + ggplot2::geom_rect(data = sev_spans, inherit.aes = FALSE,
                                      ggplot2::aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf,
                                                   fill = "Severe episode"), alpha = alpha_severe)
  }
  plot <- plot +
    ggplot2::geom_line(data = df_plot,
                       ggplot2::aes(x = start_date, y = total,   color = "Total"),   linewidth = 0.9) +
    ggplot2::geom_line(data = df_plot,
                       ggplot2::aes(x = start_date, y = non_wfs, color = "Non-WFS"), linewidth = 0.9) +
    ggplot2::geom_line(data = df_plot,
                       ggplot2::aes(x = start_date, y = wfs,     color = "WFS"),     linewidth = 0.9) 
  
  if (sum(df_plot$severe_episode_id > 0) || sum(df_plot$episode_id > 0)) {
    plot <- plot + ggplot2::scale_fill_manual(values = c("Episode" = "orange", "Severe episode" = "red3"),
                               name = NULL,
                               guide = ggplot2::guide_legend(override.aes = list(alpha = 0.4))) 
  }
  
  plot + 
    ggplot2::scale_color_manual(values = c("Total"="black","Non-WFS"=median_range_color,"WFS"=wildfire_attributable_col),
                                breaks = c("Total","Non-WFS","WFS"),
                                name   = NULL,
                                guide  = ggplot2::guide_legend(override.aes = list(fill = NA, linewidth = 1.3))) +
    ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0,0)) +
    ggplot2::scale_y_continuous(expand = c(0,0), limits = c(-0.25, y_max)) +
    ggplot2::labs(x = "Year",
                  y = expression("Mean PM"[2.5]*" ("*mu*"g/m"^3*")")) +
    .fn_plot_theme_small()
}

fn_plot_histogram <- function(df, binwidth = 8) {
  # Distribution of weekly sums with the stable-Z band highlighted
  slopes <- df$weekly_pm25_sum_i
  z_vals <- df$modified_z
  stable_idx <- which(!is.na(z_vals) & z_vals >= -2 & z_vals <= 2)
  min_slope  <- if (length(stable_idx)) min(slopes[stable_idx]) else NA_real_
  max_slope  <- if (length(stable_idx)) max(slopes[stable_idx]) else NA_real_
  tmp_hist <- ggplot2::ggplot_build(
    ggplot2::ggplot(df, ggplot2::aes(x = weekly_pm25_sum_i)) +
      ggplot2::geom_histogram(binwidth = binwidth, boundary = 0, na.rm = TRUE)
  )
  y_max_count <- max(tmp_hist$data[[1]]$count, na.rm = TRUE) * 1.1
  rect_df <- data.frame(xmin = min_slope, xmax = max_slope, ymin = 0, ymax = y_max_count)
  x_max <- max(slopes, na.rm = TRUE) * 1.05
  ggplot2::ggplot() +
    ggplot2::geom_rect(data = rect_df,
                       ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                                    fill = "-2 < Z-Score < 2"), alpha = 0.3) +
    ggplot2::geom_histogram(data = df,
                            ggplot2::aes(x = weekly_pm25_sum_i),
                            binwidth = binwidth, boundary = 0,
                            fill = "grey90", color = "black",
                            linewidth = 0.4, show.legend = FALSE,
                            na.rm = TRUE) +
    { if (!is.na(min_slope))
      ggplot2::geom_vline(xintercept = min_slope, linetype = "dashed", linewidth = 0.4) } +
    { if (!is.na(max_slope))
      ggplot2::geom_vline(xintercept = max_slope, linetype = "dashed", linewidth = 0.4) } +
    ggplot2::geom_vline(ggplot2::aes(xintercept = median(slopes, na.rm = TRUE), color = "Median"),
                        linewidth = 0.9) +
    ggplot2::scale_fill_manual(values = c("-2 < Z-Score < 2" = "yellow"), name = NULL) +
    ggplot2::scale_color_manual(values = c("Median" = "#0085E4"), name = NULL) +
    ggplot2::guides(fill = ggplot2::guide_legend(order = 1),
                    color = ggplot2::guide_legend(order = 2)) +
    ggplot2::scale_x_continuous(limits = c(0, x_max), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(0, y_max_count), expand = c(0, 0)) +
    ggplot2::labs(
      x = expression("Weekly Sum of PM"[2.5]*" ("*mu*"g/m"^3*" per epi-week)"),
      y = "Count"
    ) +
    .fn_plot_theme_small() +
    ggplot2::theme(panel.grid = ggplot2::element_blank())
}

fn_build_combined_plot <- function(id, df) {
  # Stack cumulative, weekly-average, and distribution panels into one figure
  p1 <- fn_plot_cumulative_sum_by_year(df)
  p2 <- fn_plot_time_series_weekly_mean(df)
  p3 <- fn_plot_histogram(df)
  (p1 / p2 / p3) +
    patchwork::plot_annotation(
      title = id,
      theme = ggplot2::theme(plot.title = ggplot2::element_text(size = 20, face = "bold", hjust = 0.5))
    )
}

fn_run_id_analysis <- function(dt_daily, ID) {
  # Per-ID pipeline: clean, weekly-aggregate, build trajectory, tag episodes, compute metrics
  neg_n <- sum(!is.na(dt_daily$Total_PM25) & dt_daily$Total_PM25 < 0)
  if (neg_n > 0) {
    dt_daily[Total_PM25 < 0 & !is.na(Total_PM25), Total_PM25 := NA_real_]
    warning(sprintf("Warning: The ID had %d negative value(s) in the 'Total_PM25' column. All negative values were replaced with NA before processing.", neg_n),
            call. = FALSE)
  }
  # Collapse duplicates by date using the mean, and report large discrepancies
  dup_info <- dt_daily[, .N, by = .(Date)][N > 1]
  if (nrow(dup_info)) {
    n_dup_dates <- nrow(dup_info)
    diff_gt10 <- dt_daily[Date %in% dup_info$Date,
                          { v <- Total_PM25[!is.na(Total_PM25)]
                          d <- if (length(v)) (max(v) - min(v)) else NA_real_
                          .(flag = isTRUE(d > 10)) }, by = Date][, sum(flag)]
    dt_daily <- dt_daily[, .(Total_PM25 = mean(Total_PM25, na.rm = TRUE)), by = .(Date)]
    dt_daily[, ID := ID]
    warning(sprintf("Warning: The ID had %d duplicated dates. %d of the duplicated dates had a difference >10 µg/m³ between the Total_PM25 values. For each duplicated date, Total_PM25 was set to mean value across all duplicates.",
                    n_dup_dates, diff_gt10), call. = FALSE)
  } else {
    setorder(dt_daily, Date)
  }
  
  weekly     <- fn_build_weekly_from_daily(dt_daily)
  trajectory <- fn_build_counterfactual_trajectory(weekly)
  
  # Regular episodes
  trajectory[, episode_id := fn_identify_smoke_episodes(
    is_smoke_impacted          = as.integer(dplyr::coalesce(weekly_wfs_pm25_avg_i, 0) > 0),
    cont_week                  = week_sequence,
    weekly_sum_wfs_pm25_values = weekly_wfs_pm25_sum_i,
    episode_threshold          = 0,
    num_weeks_in_episode       = 2,
    max_no_smoke_gap           = 3,
    weeks_needed_above_threshold = 1
  )]
  
  # Severe episodes
  trajectory[, severe_episode_id := fn_identify_smoke_episodes(
    is_smoke_impacted          = as.integer(dplyr::coalesce(weekly_wfs_pm25_avg_i, 0) > 0),
    cont_week                  = week_sequence,
    weekly_sum_wfs_pm25_values = weekly_wfs_pm25_sum_i,
    episode_threshold          = 250,
    num_weeks_in_episode       = 1,
    max_no_smoke_gap           = 3,
    weeks_needed_above_threshold = 1
  )]
  
  # Merge overlapping regular + severe episode spans into combined episode IDs
  episodes_combined <- rep(0L, nrow(trajectory))
  epi_counter <- 1L
  for (i in seq_len(nrow(trajectory))) {
    curr_has_epi <- trajectory$episode_id[i] > 0L || trajectory$severe_episode_id[i] > 0L
    next_has_epi <- if (i < nrow(trajectory))
      (trajectory$episode_id[i+1] > 0L || trajectory$severe_episode_id[i+1] > 0L) else FALSE
    if (curr_has_epi) {
      episodes_combined[i] <- epi_counter
      if (!next_has_epi) epi_counter <- epi_counter + 1L
    }
  }
  trajectory[, episode_id := episodes_combined]
  
  metrics <- fn_compute_metrics(
    trajectory,
    exposure_start = min(dt_daily$Date),
    exposure_end   = max(dt_daily$Date)
  )
  
  list(id = ID, metrics = metrics, data = trajectory)
}

sanitize_names <- function(x) {
  # Safe column/file names for CSVs (ASCII, underscores, no slashes/parens)
  x %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    gsub("₂", "2", ., fixed = TRUE) %>%
    gsub("₅", "5", ., fixed = TRUE) %>%
    gsub("PM2\\.5", "PM2_5", ., perl = TRUE) %>%
    gsub("[/()]", "_", ., fixed = TRUE) %>%
    gsub("[^A-Za-z0-9_]+", "_", .) %>%
    gsub("_+", "_", .) %>%
    gsub("^_|_$", "", .)
}

save_batch_outputs <- function(batch, out_dir_root) {
  # Persist metrics + weekly estimates to a timestamped run directory
  run_dir <- file.path(out_dir_root, paste0("run_", format(Sys.time(), "%Y%m%d_%H%M%S")))
  dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Metrics (wide)
  metrics_csv <- data.table::copy(batch$metrics_wide)
  data.table::setnames(metrics_csv, sanitize_names(names(metrics_csv)))
  data.table::fwrite(metrics_csv, file = file.path(run_dir, "MultiWiSE_Metrics.csv"))
  
  # Weekly estimates (long)
  long_weekly <- data.table::rbindlist(batch$data, idcol = "ID", use.names = TRUE)
  long_weekly[, epiweek_index_rel := seq_len(.N), by = ID]
  cols_keep <- c(
    "ID", "epiweek_index_rel", "n_days",
    "epiweek_start_date", "epiweek_end_date",
    "weekly_pm25_avg_i", "weekly_wfs_pm25_avg_i", "weekly_non_wfs_pm25_avg_i",
    "weekly_pm25_sum_i", "weekly_wfs_pm25_sum_i", "weekly_non_wfs_pm25_sum_i",
    "episode_id", "severe_episode_id"
  )
  long_weekly <- long_weekly[, ..cols_keep]
  data.table::setnames(long_weekly,
                       old = c("epiweek_index_rel",
                               "weekly_pm25_avg_i", "weekly_wfs_pm25_avg_i", "weekly_non_wfs_pm25_avg_i",
                               "weekly_pm25_sum_i", "weekly_wfs_pm25_sum_i", "weekly_non_wfs_pm25_sum_i"),
                       new = c("epiweek_index",
                               "weekly_pm25_avg", "weekly_wfs_pm25_avg", "weekly_non_wfs_pm25_avg",
                               "weekly_pm25_sum", "weekly_wfs_pm25_sum", "weekly_non_wfs_pm25_sum"))
  num_cols <- c("weekly_pm25_avg","weekly_wfs_pm25_avg","weekly_non_wfs_pm25_avg",
                "weekly_pm25_sum","weekly_wfs_pm25_sum","weekly_non_wfs_pm25_sum")
  long_weekly[, (num_cols) := lapply(.SD, function(x) round(x, 3)), .SDcols = num_cols]
  data.table::fwrite(long_weekly, file = file.path(run_dir, "Weekly_PM25_Estimates.csv"))
  
  return(run_dir)
}


#### UI ####
has_backend <- function() TRUE  # Present for parity/feature checks elsewhere if needed

zip_dir <- function(src_dir, zip_path) {
  # Zip contents of src_dir into zip_path
  files <- list.files(src_dir, all.files = FALSE, recursive = TRUE,
                      include.dirs = TRUE, no.. = TRUE)
  old <- setwd(src_dir); on.exit(setwd(old), add = TRUE)
  if (requireNamespace("zip", quietly = TRUE)) {
    zip::zipr(zipfile = zip_path, files = files)
  } else {
    utils::zip(zipfile = zip_path, files = files)
  }
}

required_cols <- c("ID", "Date", "Total_PM25")

check_date_format_loose <- function(date_vec) {
  # Strict check and returns both count and row indices
  x <- trimws(as.character(date_vec))
  invalid_blank <- is.na(x) | x == ""
  matches_ymd   <- grepl("^\\d{4}-\\d{2}-\\d{2}$", x)
  to_check      <- which(!invalid_blank & matches_ymd)
  parsed        <- as.Date(x[to_check], format = "%Y-%m-%d")
  roundtrip_ok  <- !is.na(parsed) & (format(parsed, "%Y-%m-%d") == x[to_check])
  invalid <- invalid_blank | !matches_ymd
  invalid[to_check] <- !roundtrip_ok
  list(n_bad = sum(invalid, na.rm = TRUE), bad_idx = which(invalid))
}

dummy_metrics <- function() tibble::tibble()[, c(), drop = FALSE]  # Empty table placeholder
dummy_weekly  <- function() tibble::tibble()[, c(), drop = FALSE]  # Empty table placeholder
dummy_log     <- function() tibble(ID = character(), Message = character())  # Empty log

theme <- bs_theme(version = 5, bootswatch = "cosmo")
theme <- bslib::bs_add_rules(theme, "
  body { padding-top: 60px; }
  @media (max-width: 991.98px) { body { padding-top: 56px; } }
  .tab-content { padding-bottom: 3rem; }
  .sidebar .btn-sm { padding: .25rem .5rem; font-size: 1em; background-color: none; }
  .results-download-bar { display:flex; justify-content:flex-end; gap:.5rem; margin-bottom:10px; }

  .muted { opacity: .25 !important; filter: saturate(0.2); pointer-events: none !important; }

  /* Hide Shiny's default fileInput progress/error strip */
  #pm_file_progress { display: none !important; }

  #profile_type .shiny-options-group { display: flex; gap: 18px; align-items: center; flex-wrap: wrap; }
  #profile_type .form-check-inline { margin-right: 18px; }
  #profile_type .form-check-inline:last-child { margin-right: 0; }

  /* Issues badge */
  #warn_badge {
    display: none;
    font-weight: 600;
    width: 1.25rem; height: 1.25rem;
    border-radius: 50% !important; padding: 0;
    display: inline-flex; align-items: center; justify-content: center;
    line-height: 1; font-size: 0.85rem;
  }

  /* Inline error (single red line under chooser) */
  .inline-error {
    color: #B00020;
    font-weight: 400;
    font-size: 12.5px;
    line-height: 1.25;
    margin: 0;
  }
  .inline-error .err-title {
    text-transform: uppercase;
    letter-spacing: .3px;
    margin-right: 6px;
    font-weight: 500;
  }
  .upload-msg {
    display: block;
    margin-top: 0;        /* hug the chooser */
    margin-bottom: 0;
    line-height: 1.2;
  }
  
  #clear_file { 
  margin-top: 10px !important;   /* try 12–16px if you want more space */}

  /* Tighten spacing around the file chooser + message */
  #pm_file { margin-bottom: 0 !important; }
  #pm_file.shiny-input-container { margin-bottom: 0 !important; }
  #pm_file .form-label { margin-bottom: 2px !important; }
  #pm_file .input-group { margin-bottom: 0 !important; }
  #pm_file .form-control { margin-bottom: 0 !important; }
  #size_error { margin-top: -1 !important; }   /* container that holds the inline error */
")


ui <- navbarPage(
  title = HTML("Multi<b>WiSE</b> Dashboard"),
  theme = theme,
  collapsible = TRUE,
  windowTitle = "MultiWiSE Dashboard",
  id = "topnav",
  position = "fixed-top",
  
  tabPanel(
    "Calculate Metrics",
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 360,
        open = "always",
        sticky = TRUE,
        
        useShinyjs(),
        h3("Upload Data"),
        tags$p(HTML("Upload a multiyear time series of daily total (all-source) PM<sub>2.5</sub>.")),
        tags$p("Required columns: ", tags$code("ID, Date, Total_PM25")),
        
        tags$form(id = "upload_form",
                  fileInput(
                    "pm_file", label = "Select a .csv/.xls/.xlsx file",
                    accept = c(".csv", ".xls", ".xlsx"),
                    buttonLabel = "Select a file",
                    placeholder = "No file selected"
                  ),
                  # Server-rendered slot (kept for the inline-size message)
                  uiOutput("size_error"),
                  
                  # Client hook: blocks oversize uploads & notifies server (no visible text here)
                  tags$script(HTML("
(function(){
  var MAX = 5000e6; // NOTE: 5 GB Max upload size for locally run dashboard

  function clearChooser(){
    var wrap = document.getElementById('pm_file');
    if (!wrap) return;
    var finput = wrap.querySelector('input[type=file]');
    if (finput) finput.value = '';
    var mirror = wrap.querySelector('input[type=text]');
    if (mirror) mirror.value = '';
  }

  function attachOnce(){
    var finput = document.querySelector('#pm_file input[type=file], input[type=file]#pm_file');
    if (!finput || finput.dataset.bound === '1') return;

    finput.addEventListener('change', function(){
      var f = this.files && this.files[0];
      if (f && Number.isFinite(f.size) && f.size > MAX){
        clearChooser();
        if (window.Shiny && Shiny.setInputValue){
          Shiny.setInputValue('client_upload_error',
            { name: f.name, size: f.size, ts: Date.now() }, { priority: 'event' });
        }
      }
    }, false);

    finput.dataset.bound = '1';
  }

  document.addEventListener('DOMContentLoaded', attachOnce);
  document.addEventListener('shiny:connected', attachOnce);
  document.addEventListener('shiny:value', attachOnce);
  var mo = new MutationObserver(function(){ attachOnce(); });
  mo.observe(document.documentElement, { subtree: true, childList: true });

  document.addEventListener('click', function(ev){
    if (ev.target && ev.target.id === 'clear_file'){
      clearChooser();
    }
  }, false);
})();
"))
                  ,
                  actionButton("clear_file", "Clear", class = "btn btn-sm btn-outline-secondary")
        ),
        
        hr(),
        h3("Customize Metrics"),
        div(
          style = "max-width:320px;",
          numericInput(
            "metric6_threshold",
            label = HTML("Metric 6: Total PM<sub>2.5</sub> > <b>?</b> µg/m³"),
            value = 25, min = 0, step = 1
          )
        ),
        tags$details(
          tags$summary("Show metric list"),
          tags$small(HTML(paste(
            c(
              "1. Cumulative WFS PM<sub>2.5</sub> (mg/m³)",
              "2. WFS Fraction (%)",
              "3. Average WFS PM<sub>2.5</sub> (µg/m³)",
              "4. Any WFS (# weeks)",
              "5. WFS PM<sub>2.5</sub> > 5 (# weeks)",
              "6. Total PM<sub>2.5</sub> > threshold (# weeks)",
              "7. WFS Episodes (# episodes)",
              "8. Severe Episodes (# episodes)",
              "9. Longest Episode (# weeks)",
              "10. Worst Episode (µg/m³)",
              "11. WFS from Severe Episodes (%)",
              "12. Average Recovery (# weeks)"
            ),
            collapse = "<br>")))
        ),
        
        div(style = "margin-top:16px;"),
        shinyjs::disabled(
          actionButton("run_analysis", "Calculate Metrics", class = "btn btn-primary btn-lg", width = "100%")
        ),
        tags$small(textOutput("status_line"), style="display:block; margin-top:6px;")
      ),
      
      h3("Results", id = "results_hdr", class = "muted"),
      uiOutput("run_alert"),
      div(
        id = "download_bar",
        class = "results-download-bar muted",
        style = "margin: 6px 0 8px 0;",
        tagList(
          downloadButton("dl_all_zip", "Download results (.zip)"),
          tags$script(HTML("
            document.addEventListener('DOMContentLoaded', function(){
              var b = document.getElementById('dl_all_zip');
              if (b) b.addEventListener('click', function(){
                Shiny.setInputValue('zip_clicked', Date.now(), {priority:'event'});
              });
            });
          "))
        )
      ),
      
      div(
        id = "results_wrap",
        class = "muted",
        tabsetPanel(id = "results_tabs",
                    tabPanel("MultiWiSE Metrics",
                             br(),
                             DTOutput("tbl_metrics")
                    ),
                    tabPanel(HTML("Weekly PM<sub>2.5</sub> estimates"),
                             br(),
                             DTOutput("tbl_weekly")
                    ),
                    tabPanel("Figures",
                             br(),
                             div(style = "display:flex; gap:10px; align-items:center; justify-content:space-between; flex-wrap:wrap;",
                                 radioButtons(
                                   "profile_type", "Profile", inline = TRUE,
                                   choiceNames = list(
                                     "Cumulative Exposure",
                                     "Weekly Average Exposure",
                                     HTML("Distribution of Weekly Sum of Total PM<sub>2.5</sub>")
                                   ),
                                   choiceValues = list("cumulative", "weekly_avg", "counterfactual_hist"),
                                   selected = "cumulative"
                                 ),
                                 uiOutput("id_selector_ui")
                             ),
                             plotOutput("placeholder_plot", height = "420px"),
                             br(),
                             downloadButton("dl_selected_plot", "Download figure (selected ID)")
                    ),
                    tabPanel(
                      title = tagList(
                        "Warning/Error Log",
                        tags$span(
                          id = "warn_badge",
                          class = "badge rounded-pill bg-secondary ms-2",
                          "0"
                        )
                      ),
                      br(),
                      DTOutput("tbl_log")
                    )
        )
      )
    ),
    
    # Floating upload indicator
    tags$div(
      id = "upload_busy",
      class = "card shadow-sm",
      style = "position: fixed; right: 18px; bottom: 18px; z-index: 1060; display: none; padding: 10px 12px;",
      tags$div(
        style = "display:flex; align-items:center; gap:10px;",
        tags$div(class = "spinner-border spinner-border-sm", role = "status", `aria-hidden` = "true"),
        tags$div(
          tags$b("Preparing your file…"),
          tags$br(),
          tags$small(id = "upload_busy_text", "Reading upload")
        )
      )
    )
  ),
  
  # About tab
  tabPanel(
    "About",
    fluidPage(
      fluidRow(
        column(
          width = 9,
          
          h4(HTML("<b>About the Dashboard</b>")),
          HTML("
<p>This dashboard uses multiyear time series of daily total (i.e., all-source) PM<sub>2.5</sub> to generate weekly estimates of wildfire smoke (WFS) PM<sub>2.5</sub> and calculate the 12 Multiyear Wildfire Smoke Exposure (MultiWiSE) metrics that characterize episodic exposure to WFS PM<sub>2.5</sub>. The dashboard can be used to generate WFS exposure profiles and the MultiWiSE metrics for multiple identifiers (IDs), so long as a multiyear PM<sub>2.5</sub> time series is provided for each ID. An ID can represent a geographic location (e.g., postal or ZIP code) or an individual and is used to group the daily PM<sub>2.5</sub> concentrations into a single time series. For each ID, the dashboard will calculate the exposure profile and MultiWiSE metrics for the entire duration of the provided time series. An overview of the required input to the dashboard can be found below in ‘Usage’, and an overview of the provided outputs can be found in ‘Outputs.’ Additional details on the dashboard can be found in the README.</p>
"),
          
          h4(HTML("<b>Usage</b>")),
          HTML("
<p>The dashboard relies on the user providing a multiyear time series of daily total (i.e., all-source) PM<sub>2.5</sub> for each ID. The time series for all IDs must be uploaded to the dashboard in a single file in .csv, .xlsx, or .xls format and include the following columns: ID, Date, Total_PM25. For the dashboard to work as expected, the uploaded file should meet the following conditions:</p>
<ul>
  <li>All dates must be written as YYYY-MM-DD (e.g., 2022-07-15).</li>
  <li>The Total_PM25 column must represent PM<sub>2.5</sub> from all sources (i.e., not WFS-specific PM<sub>2.5</sub>).</li>
  <li>The PM<sub>2.5</sub> values provided in the Total_PM25 column must be &ge; 0 &micro;g/m&sup3;.</li>
  <li>One PM<sub>2.5</sub> concentration must be provided for each ID–Date combination (i.e., no duplicates).</li>
  <li>The time series for a given ID should be contiguous, with no missing dates or PM<sub>2.5</sub> values.</li>
  <li>The time series for each ID must be at least 1 year in length and include at least 365 days of PM<sub>2.5</sub> data.</li>
</ul>
<p>If any of the above conditions are not met, the dashboard will either: (1) automatically handle the issue and store an associated warning message detailing how the issue was addressed, or (2) stop running entirely or skip the problematic ID(s) and store an error message detailing what the issue was.</p>
"),
          tags$h5(HTML("<i>Running the dashboard using larger files</i>")),
          HTML("
<p>If you would like to process a file that is greater than 250 MB in size, you can run the dashboard locally on your machine. To do so, download the code and files provided here: <a href='https://github.com/sfu-fhs-cleland/MultiWiSE_Dashboard' target='_blank'>https://github.com/sfu-fhs-cleland/MultiWiSE_Dashboard</a>. You can then open app.R in RStudio, hit 'Run App,' and proceed with processing your larger files.</p>"),
          
          
          h4(HTML("<b>Outputs</b>")),
          HTML("
<p>There are four ID-level outputs provided by the dashboard. Each output is calculated over the duration of the provided time series:</p>
"),
          HTML('
<ol>
  <li><strong>12 MultiWiSE metrics</strong> and five additional variables.</li>
  <li>Three figures of <b>PM<sub>2.5</sub> exposure profiles</b>:
    <ul>
      <li>Cumulative PM<sub>2.5</sub> exposure, separated into total, WFS, and non-WFS components.</li>
      <li>Weekly average total, WFS, and non-WFS PM<sub>2.5</sub> concentrations, with identification of WFS episodes and severe WFS episodes.</li>
      <li>Distribution of the weekly sum of total PM<sub>2.5</sub> exposure, with indication of the range and median used to identify the counterfactual weekly value.</li>
    </ul>
  </li>
  <li><strong>Weekly PM<sub>2.5</sub> estimates</strong> (total, WFS, and non-WFS PM<sub>2.5</sub> and WFS episodes).</li>
  <li><b>Log file</b> with warning and error messages.</li>
</ol>'),
          # Figure block
          tags$div(
            style = "margin-top:12px; text-align:center;",
            tags$a(
              href = "Figure2.png", target = "_blank",
              tags$img(
                src = "Figure2.png",
                alt = "Overview of the 12 MultiWiSE metrics.",
                style = "max-width:80%; height:auto; border:1px solid #eee; border-radius:8px;"
              )
            ),
            tags$div(
              style = "font-size:0.9em; color:#555; margin-top:6px;",
              HTML("Overview of the 12 MultiWiSE metrics.")
            )
          ),
          
          h4(HTML("<b>Support</b>")),
          HTML('<p>If you require further support or have questions regarding this dashboard, please contact Dr. Stephanie Cleland (<a href="mailto:stephanie_cleland@sfu.ca">stephanie_cleland@sfu.ca</a>) or Dr. Sarah Henderson (<a href="mailto:sarah.henderson@bccdc.ca">sarah.henderson@bccdc.ca</a>).</p>')
        ),
        
        column(
          width = 3,
          div(
            style = "position: sticky; top: 84px; width: 18em;",
            tags$div(
              class = "card shadow-sm",
              style = "padding:16px;",
              tags$p("Download the full README"),
              tags$a(
                href = "ReadMe_102225.pdf", target = "_blank", download = NA,
                download = "MultiWiSE_Dashboard_README.pdf",
                class = "btn btn-primary btn-lg w-100",
                HTML("<b>Download README (PDF)</b>")
              )
            )
          )
        )
      )
    )
  )
  
  
)



####  SERVER   ####

server <- function(input, output, session) {
  # Centralized message for oversize uploads (used in multiple places)
  # OVERSIZE_MSG <- "Maximum upload size is 250 MB. Please see 'About' for how to run the dashboard using larger files." # NOTE: No upload cap for running dashboard locally
  
  v <- reactiveValues(
    ids = character(0),
    data_by_id = list(),
    metrics_df = NULL,
    weekly_df  = NULL,
    log_df     = NULL,
    file_ok    = FALSE,
    last_run_dir = NULL
  )
  
  reset_results_view <- function() {
    # Clear tables/plots and mute the results area
    shinyjs::addClass("results_wrap", "muted")
    shinyjs::addClass("results_hdr",  "muted")
    shinyjs::hide("warn_badge")
    output$tbl_metrics <- DT::renderDT({
      DT::datatable(dummy_metrics(), rownames = FALSE, options = list(pageLength = 10),
                    caption = htmltools::tags$caption(style='caption-side: top; text-align:left;', ''))
    })
    output$tbl_weekly <- DT::renderDT({
      DT::datatable(dummy_weekly(), rownames = FALSE, options = list(pageLength = 10),
                    caption = htmltools::tags$caption(style='caption-side: top; text-align:left;', ''))
    })
    output$tbl_log <- DT::renderDT({
      DT::datatable(dummy_log(), rownames = FALSE, options = list(pageLength = 10))
    })
    output$id_selector_ui <- renderUI({
      div(class = "text-muted", HTML("Select an ID will appear here after you calculate metrics."))
    })
    output$placeholder_plot <- renderPlot({
      par(mar = c(0,0,0,0)); plot.new()
      text(0.5, 0.55, "No figures yet", cex = 1.5)
      text(0.5, 0.45, "Upload a file and click \"Calculate Metrics\".")
    })
  }
  
  # Render a compact inline error (red text under the chooser)
  make_alert <- function(msg, title = "Error", class = "inline-error") {
    htmltools::div(
      class = class,
      if (!is.null(title) && nzchar(title))
        htmltools::span(class = "err-title", paste0(title,":")),
      htmltools::span(as.character(msg))
    )
  }
  
  update_warn_badge <- function(issues) {
    # Show/hide the count on the Warning/Error Log tab
    shinyjs::html("warn_badge", as.character(issues))
    if (isTRUE(issues > 0)) shinyjs::show("warn_badge") else shinyjs::hide("warn_badge")
  }
  
  # Initial UI state
  shinyjs::disable("run_analysis")
  shinyjs::disable("dl_all_zip")
  shinyjs::addClass("download_bar", "muted")
  reset_results_view()
  output$run_alert <- renderUI(NULL)
  output$size_error <- renderUI(NULL)
  output$status_line <- renderText("")
  shinyjs::hide("upload_busy")
  
  observeEvent(input$client_upload_error, ignoreInit = TRUE, {
    # Client-side size gate fired: reset the chooser and show inline message
    shinyjs::reset("upload_form")
    v$ids <- character(0); v$file_ok <- FALSE; v$last_run_dir <- NULL
    shinyjs::disable("run_analysis")
    shinyjs::disable("dl_all_zip")
    shinyjs::addClass("download_bar", "muted")
    output$size_error <- renderUI(make_alert("", class = "inline-error upload-msg"))
    output$run_alert <- renderUI(NULL)
    output$status_line <- renderText("")
    shinyjs::hide("upload_busy")
    reset_results_view()
    # No toast—to avoid duplicate messaging
  })
  
  observeEvent(input$clear_file, ignoreInit = TRUE, {
    # Clear the chooser and reset everything related to results
    shinyjs::reset("upload_form")
    v$ids <- character(0)
    v$file_ok <- FALSE
    v$last_run_dir <- NULL
    shinyjs::disable("run_analysis")
    shinyjs::disable("dl_all_zip")
    shinyjs::addClass("download_bar", "muted")
    output$size_error <- renderUI(NULL)
    output$run_alert <- renderUI(NULL)
    output$status_line <- renderText("")
    shinyjs::hide("upload_busy")
    reset_results_view()
  })
  
  # ---------- Upload observer with size gate ----------
  observeEvent(input$pm_file, {
    # Validate file immediately; block oversize and bad columns/dates early
    req(input$pm_file)
    output$run_alert <- renderUI(NULL)
    output$size_error <- renderUI(NULL)
    shinyjs::disable("run_analysis")
    shinyjs::disable("dl_all_zip")
    shinyjs::addClass("download_bar", "muted")
    v$file_ok <- FALSE
    
    # Double-check file size (reported and on-disk)
    # NOTE: No upload cap for running dashboard locally
    # reported_size <- if (!is.null(input$pm_file$size) && length(input$pm_file$size)) input$pm_file$size[1] else NA_real_
    # disk_size <- tryCatch({
    #   p <- input$pm_file$datapath
    #   if (!is.null(p) && nzchar(p) && file.exists(p)) file.info(p)$size else NA_real_
    # }, error = function(e) NA_real_)
    # actual_size <- suppressWarnings(if (is.finite(reported_size)) reported_size else disk_size)
    # if (is.finite(actual_size) && actual_size > MAX_UPLOAD_BYTES) {
    #   # Server-side backstop mirrors the client-side block
    #   shinyjs::reset("upload_form")
    #   v$ids <- character(0); v$file_ok <- FALSE; v$last_run_dir <- NULL
    #   shinyjs::disable("run_analysis")
    #   shinyjs::disable("dl_all_zip")
    #   shinyjs::addClass("download_bar", "muted")
    #   output$size_error <- renderUI(make_alert(OVERSIZE_MSG, class = "inline-error upload-msg"))
    #   output$run_alert <- renderUI(NULL)
    #   output$status_line <- renderText("")
    #   shinyjs::hide("upload_busy")
    #   reset_results_view()
    #   return(invisible())
    # }
    
    # Read + basic validation
    shinyjs::html("upload_busy_text", "Reading upload")
    shinyjs::show("upload_busy")
    on.exit(shinyjs::hide("upload_busy"), add = TRUE)
    
    path <- input$pm_file$datapath
    ext  <- tolower(tools::file_ext(path))
    
    df <- tryCatch({
      if (ext == "csv") {
        data.table::fread(path, nThread = 1, showProgress = FALSE)
      } else if (ext %in% c("xls", "xlsx")) {
        shinyjs::html("upload_busy_text", "Reading Excel file")
        readxl::read_excel(path)
      } else {
        stop(sprintf("Unsupported extension .%s. Please upload a .csv, .xls, or .xlsx file.", ext), call. = FALSE)
      }
    }, error = function(e) {
      output$run_alert <- renderUI(make_alert(conditionMessage(e)))
      return(NULL)
    })
    if (is.null(df)) { output$status_line <- renderText(""); return() }
    
    shinyjs::html("upload_busy_text", "Validating columns")
    if (!all(required_cols %in% names(df))) {
      output$run_alert <- renderUI(make_alert(
        "Unable to process data. The provided file does not have the correct column names. Please use the following column names: ID, Date, Total_PM25."
      ))
      output$status_line <- renderText("")
      return()
    }
    
    shinyjs::html("upload_busy_text", "Checking date formats")
    chk <- tryCatch(check_date_format_loose(df$Date), error = function(e) list(n_bad = NA_integer_))
    if (is.na(chk$n_bad)) {
      output$run_alert <- renderUI(make_alert("Unable to read the Date column. Make sure dates are in a recognizable text format."))
      output$status_line <- renderText("")
      return()
    }
    if (chk$n_bad > 0) {
      output$run_alert <- renderUI(make_alert(
        sprintf("Unable to process data. The Date column in the provided file has %d date(s) that are incorrectly formatted or missing. Please ensure all dates are formatted as follows: YYYY-MM-DD.", chk$n_bad)
      ))
      output$status_line <- renderText("")
      return()
    }
    
    # Good to proceed
    shinyjs::html("upload_busy_text", "Preparing IDs")
    v$ids <- unique(as.character(df$ID))
    v$file_ok <- TRUE
    shinyjs::enable("run_analysis")
    output$size_error <- renderUI(NULL)
    output$status_line <- renderText("File looks good. Ready to calculate.")
    reset_results_view()
  })
  
  # ---------- Calculate Metrics with size gate ----------
  observeEvent(input$run_analysis, {
    tryCatch({
      req(v$file_ok, input$pm_file)
      
      # Second size check right before compute (belt-and-suspenders)
      # NOTE: No upload cap for running dashboard locally
      # p <- input$pm_file$datapath
      # s <- tryCatch(if (!is.null(p) && nzchar(p) && file.exists(p)) file.info(p)$size else NA_real_,
      #               error = function(e) NA_real_)
      # if (is.finite(s) && s > MAX_UPLOAD_BYTES) {
      #   output$run_alert <- renderUI(NULL)
      #   # output$size_error <- renderUI(make_alert(OVERSIZE_MSG, class = "inline-error upload-msg")) # NOTE: No upload cap for running dashboard locally
      #   output$status_line <- renderText("")
      #   shinyjs::disable("run_analysis")
      #   return(invisible())
      # }
      # 
      output$run_alert <- renderUI(NULL)
      shinyjs::disable("dl_all_zip")
      shinyjs::addClass("download_bar", "muted")
      shinyjs::addClass("results_wrap", "muted")
      shinyjs::addClass("results_hdr",  "muted")
      update_warn_badge(0)
      output$status_line <- renderText("Calculating...")
      
      withProgress(message = "Calculating metrics...", value = 0, {
        # Read + strict validation inside the compute step
        dt <- tryCatch(
          read_and_validate_daily_file(input$pm_file$datapath),
          error = function(e) { output$run_alert <- renderUI(make_alert(conditionMessage(e))); NULL }
        )
        if (is.null(dt)) { output$status_line <- renderText(""); return(invisible()) }
        
        # Respect the IDs discovered earlier
        if (length(v$ids)) dt <- dt[ID %in% v$ids]
        all_ids <- unique(as.character(dt$ID))
        if (!length(all_ids)) {
          output$run_alert <- renderUI(make_alert("No IDs found in the uploaded file."))
          output$status_line <- renderText("")
          return(invisible())
        }
        
        # Persist the UI-provided threshold to the analysis environment
        total_pm25_threshold_default <<- as.numeric(input$metric6_threshold %||% 25)
        
        results   <- list()
        data_list <- list()
        log_dt    <- data.table::data.table(ID = all_ids, Message = rep("No issues", length(all_ids)))
        
        # Process each ID, catching and collecting warnings/errors
        n <- length(all_ids)
        i <- 0L
        for (id in all_ids) {
          i <- i + 1L
          incProgress(1/n, detail = sprintf("[%d of %d] IDs processed...", i, n))
          d <- dt[ID == id]
          warns <- character(); errs <- character()
          
          res <- tryCatch(
            withCallingHandlers(
              fn_run_id_analysis(d, ID = id),
              warning = function(w) {
                m <- conditionMessage(w)
                if (!grepl("warnings? in .*summarise\\(", m, ignore.case = TRUE)) warns <<- c(warns, m)
                invokeRestart("muffleWarning")
              }
            ),
            error = function(e) { errs <<- c(errs, conditionMessage(e)); NULL }
          )
          
          msg <- if (length(errs)) paste(unique(errs), collapse = "; ")
          else if (length(warns)) paste(unique(warns), collapse = "; ")
          else "No issues"
          log_dt[ID == id, Message := msg]
          
          if (!is.null(res)) {
            results[[id]]   <- res
            data_list[[id]] <- res$data
          }
        }
        
        output$status_line <- renderText("Generating tables...")
        setProgress(message = "Generating tables...", detail = "Preparing outputs...")
        
        # Consolidate and save out
        if (length(results)) {
          metrics_wide <- data.table::rbindlist(
            lapply(results, function(x) {
              df <- x$metrics
              df$ID <- x$id
              data.table::as.data.table(df)[, c("ID", setdiff(names(df), "ID")), with = FALSE]
            }),
            use.names = TRUE, fill = TRUE
          )
          
          v$last_run_dir <- save_batch_outputs(
            list(metrics_wide = metrics_wide, data = data_list),
            out_dir_root = out_dir_default
          )
          
          # Prepare weekly (long form) for on-screen table
          long_weekly <- data.table::rbindlist(data_list, idcol = "ID", use.names = TRUE)
          long_weekly[, epiweek_index_rel := seq_len(.N), by = ID]
          cols_keep <- c(
            "ID", "epiweek_index_rel", "n_days",
            "epiweek_start_date", "epiweek_end_date",
            "weekly_pm25_avg_i", "weekly_wfs_pm25_avg_i", "weekly_non_wfs_pm25_avg_i",
            "weekly_pm25_sum_i", "weekly_wfs_pm25_sum_i", "weekly_non_wfs_pm25_sum_i",
            "episode_id", "severe_episode_id"
          )
          long_weekly <- long_weekly[, ..cols_keep]
          data.table::setnames(
            long_weekly,
            old = c("epiweek_index_rel",
                    "weekly_pm25_avg_i", "weekly_wfs_pm25_avg_i", "weekly_non_wfs_pm25_avg_i",
                    "weekly_pm25_sum_i", "weekly_wfs_pm25_sum_i", "weekly_non_wfs_pm25_sum_i"),
            new = c("epiweek_index",
                    "weekly_pm25_avg", "weekly_wfs_pm25_avg", "weekly_non_wfs_pm25_avg",
                    "weekly_pm25_sum", "weekly_wfs_pm25_sum", "weekly_non_wfs_pm25_sum")
          )
          num_cols <- c("weekly_pm25_avg","weekly_wfs_pm25_avg","weekly_non_wfs_pm25_avg",
                        "weekly_pm25_sum","weekly_wfs_pm25_sum","weekly_non_wfs_pm25_sum")
          long_weekly[, (num_cols) := lapply(.SD, function(x) round(x, 3)), .SDcols = num_cols]
          
          v$metrics_df <- as.data.frame(metrics_wide)
          v$weekly_df  <- as.data.frame(long_weekly)
          v$data_by_id <- data_list
          v$ids        <- names(data_list)
          v$log_df     <- as.data.frame(log_dt)
        } else {
          v$metrics_df <- NULL
          v$weekly_df  <- NULL
          v$data_by_id <- list()
          v$ids        <- character(0)
          v$log_df     <- as.data.frame(log_dt)
        }
      })
      
      # Render results
      output$tbl_metrics <- DT::renderDT({
        req(v$metrics_df)
        DT::datatable(v$metrics_df, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
      })
      output$tbl_weekly <- DT::renderDT({
        req(v$weekly_df)
        DT::datatable(v$weekly_df, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
      })
      output$tbl_log <- DT::renderDT({
        req(v$log_df)
        DT::datatable(v$log_df, rownames = FALSE, options = list(pageLength = 10))
      })
      
      output$id_selector_ui <- renderUI({
        req(length(v$ids) > 0)
        selectizeInput("sel_id", "ID", choices = v$ids, selected = v$ids[[1]], width = "320px", options = list(maxOptions = 100000))
      })
      
      output$placeholder_plot <- renderPlot({
        req(length(v$ids) > 0, input$sel_id, input$profile_type)
        df <- v$data_by_id[[input$sel_id]]; req(!is.null(df))
        if (input$profile_type == "cumulative") {
          print(fn_plot_cumulative_sum_by_year(df))
        } else if (input$profile_type == "weekly_avg") {
          print(fn_plot_time_series_weekly_mean(df))
        } else {
          print(fn_plot_histogram(df))
        }
      }, res = 120)
      
      # Final UI unmute + notifications
      shinyjs::removeClass("results_wrap", "muted")
      shinyjs::removeClass("results_hdr",  "muted")
      n_issues <- sum(v$log_df$Message != "No issues", na.rm = TRUE)
      update_warn_badge(n_issues)
      output$status_line <- renderText("Done.")
      
      if (n_issues > 0) {
        showNotification(
          sprintf("Completed. %d ID(s) had warnings/errors. See the Warning/Error Log tab.", n_issues),
          type = "warning", duration = 8
        )
      } else {
        showNotification("Completed. No issues detected.", type = "message", duration = 5)
      }
      
      # Enable ZIP if we have outputs
      if (!is.null(v$last_run_dir) && dir.exists(v$last_run_dir) && length(v$ids) > 0) {
        shinyjs::enable("dl_all_zip")
        shinyjs::removeClass("download_bar", "muted")
      } else {
        shinyjs::disable("dl_all_zip")
        shinyjs::addClass("download_bar", "muted")
      }
      
    }, error = function(e) {
      output$run_alert <- renderUI(make_alert(conditionMessage(e)))
      output$status_line <- renderText("")
    })
  })
  
  output$dl_selected_plot <- downloadHandler(
    filename = function() {
      # Name the figure with the selected ID + timestamp
      id <- input$sel_id %||% "selected"
      ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
      sprintf("MultiWiSE_Figure_%s_%s.png", id, ts)
    },
    content = function(file) {
      req(input$sel_id, v$data_by_id[[input$sel_id]])
      g <- fn_build_combined_plot(input$sel_id, v$data_by_id[[input$sel_id]])
      ggplot2::ggsave(filename = file, plot = g, width = 14, height = 28, dpi = 300)
    }
  )
  
  output$dl_all_zip <- downloadHandler(
    filename = function() {
      # Timestamped bundle of CSVs and the log
      ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
      sprintf("MultiWiSE_Results_%s.zip", ts)
    },
    content = function(file) {
      # Build a staging folder and zip it
      staging <- file.path(tempdir(), paste0("mw_bundle_", as.integer(Sys.time()), "_", Sys.getpid()))
      on.exit(try(unlink(staging, recursive = TRUE, force = TRUE), silent = TRUE), add = TRUE)
      dir.create(staging, recursive = TRUE, showWarnings = FALSE)
      
      if (!is.null(v$last_run_dir) && dir.exists(v$last_run_dir)) {
        run_files <- list.files(v$last_run_dir, full.names = TRUE, all.files = FALSE, no.. = TRUE)
        if (length(run_files)) file.copy(run_files, to = staging, recursive = FALSE, overwrite = TRUE)
      }
      
      if (!is.null(v$log_df)) {
        data.table::fwrite(as.data.table(v$log_df), file = file.path(staging, "Warning_Error_Log.csv"))
      }
      
      zip_dir(staging, zip_path = file)
    }
  )
  
  observeEvent(input$zip_clicked, ignoreInit = TRUE, {
    showNotification("Preparing ZIP...", type = "message", duration = 3)
  })
}


#### Run the app ####
shinyApp(ui = ui, server = server)
