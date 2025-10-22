# MultWiSE Canada Dashboard
# Updated Oct 22, 2025

#### Configuration and setup ####
options(shiny.maxRequestSize = 1000e6)  # 1 GB upload cap for dashboard to run locally
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
  library(rdrop2)
})

`%||%` <- function(x, y) if (is.null(x)) y else x  # Null-coalescing helper: return y when x is NULL

httr::set_config(httr::timeout(960))  # Generous HTTP timeout for remote I/O

PLOT_BASE_SIZE   <- 12   # Fixed base font size across figures
PLOT_LINE_THIN   <- 0.25 # Thin axis/line strokes for clean look
PROFILE_PLOT_HEIGHT_PX <- 560  # Taller canvas for longer y-axis

# --- Output directory root (used when bundling results) ---
out_dir_default <- getOption(
  "multiwise.out_dir",
  Sys.getenv("MW_OUT_DIR", file.path(tempdir(), "multiwise_runs"))
)
dir.create(out_dir_default, recursive = TRUE, showWarnings = FALSE)

resolve_token_path <- function() {
  # Resolve the Dropbox token from env var or local file
  env <- Sys.getenv("DROPBOX_TOKEN_RDS", unset = NA_character_)
  if (!is.na(env) && nzchar(env) && file.exists(env)) return(env)
  if (file.exists("dropbox_token 1.RDS")) return("dropbox_token 1.RDS")
  NA_character_
}

# ---- Hard cap for uploads (server-side enforcement to match 250 MB) ----
# MAX_UPLOAD_BYTES <- 250e6 # NOTE: No upload cap for running dashboard locally

#### 0  Utility helpers --------------------------------------------------------
# Read helper with strict file/type checks and robust date validation
read_and_check <- function(path,
                           required_cols,
                           file_label = c("Residential History", "Exposure Window")[1],
                           date_cols  = NULL) {
  
  # ---------- 1. Basic file checks -------------------------------------------
  allowed_ext <- c("csv", "xls", "xlsx")
  ext <- tolower(tools::file_ext(path))
  if (!ext %in% allowed_ext) {
    stop(sprintf(
      "The provided %s file is not a .csv, .xls, or .xlsx file. Please ensure that both the Residential History and Exposure Window files are provided in .csv, .xls, or .xlsx.",
      basename(path)
    ), call. = FALSE)
  }
  
  if (!file.exists(path))
    stop("Provided file path does not exist.", call. = FALSE)
  
  # ---------- 2. Read with dates as TEXT to avoid silent mis-parsing ----------
  df <- switch(
    ext,
    csv  = data.table::fread(path, colClasses = "character"),             # all text
    xls  = readxl::read_excel(path, col_types = "text"),                  # all text
    xlsx = readxl::read_excel(path, col_types = "text")                   # all text
  )
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  
  # ---------- 3. Header validation -------------------------------------------
  if (!all(required_cols %in% names(df))) {
    stop(sprintf(
      "The %s file does not have the correct column names. Please use the following column names for the %s file: %s.",
      file_label, file_label, paste(required_cols, collapse = ", ")
    ), call. = FALSE)
  }
  
  # ---------- 4. Date-string validation --------------------------------------
  if (is.null(date_cols)) {
    date_cols <- grep("Date$", names(df), value = TRUE)
  }
  
  if (length(date_cols)) {
    bad_by_col <- vapply(
      date_cols,
      function(nm) check_date_format(df[[nm]]),
      integer(1)
    )
    bad_cols  <- names(bad_by_col)[bad_by_col > 0]
    bad_total <- sum(bad_by_col)
    
    if (bad_total > 0) {
      col_list <- switch(
        length(bad_cols),
        `1` = bad_cols,
        `2` = paste(bad_cols, collapse = " and "),
        paste0(paste(bad_cols[-length(bad_cols)], collapse = ", "),
               ", and ", bad_cols[length(bad_cols)])
      )
      
      stop(sprintf(
        paste(
          "Unable to process data.",
          "The %s column%s in the %s file has %d date(s) that are incorrectly formatted or missing.",
          "Please ensure all dates are formatted as follows: YYYY-MM-DD."
        ),
        col_list,
        ifelse(length(bad_cols) > 1, "s", ""),
        file_label,
        bad_total
      ), call. = FALSE)
    }
  }
  
  # ---------- 5. Return -------------------------------------------------------
  df
}

# Count invalid entries in a date vector; enforce YYYY-MM-DD and real calendar dates
check_date_format <- function(date_vec) {
  x <- trimws(as.character(date_vec))
  invalid_blank <- is.na(x) | x == ""
  matches_ymd <- grepl("^\\d{4}-\\d{2}-\\d{2}$", x)
  to_check <- which(!is.na(x) & matches_ymd)
  parsed   <- as.Date(x[to_check], format = "%Y-%m-%d")
  roundtrip_ok <- !is.na(parsed) & (format(parsed, "%Y-%m-%d") == x[to_check])
  invalid <- invalid_blank | !matches_ymd
  invalid[to_check] <- !roundtrip_ok
  sum(invalid, na.rm = TRUE)
}

# Standardized postal-code error message
postal_code_error <- function(postal_code) {
  sprintf(
    "Error: Unable to process individual. The postal code %s provided in the residential history is incorrectly formatted and/or does not link to available data. Please ensure all postal codes included in the residential history file are correctly formatted (6 digits, no spaces) and located in one of the 13 provinces and territories in Canada.",
    postal_code
  )
}

#### 1  Build individual exposure history #####################################
fn_build_individual_exposure_history <- function(residential_history_table,
                                                 exposure_window_start,
                                                 exposure_window_end,
                                                 dropbox_token_file) {
  
  # ---- 0. Basic validations --------------------------------------------------
  if (!is.data.frame(residential_history_table))
    stop("Unable to process data. The provided residential history is not a data.frame. Please ensure you passed in a residential_history_table with columns PostalCode, StartDate, EndDate.")
  
  bad_pc_fmt <- !grepl("^[A-Za-z]\\d[A-Za-z]\\d[A-Za-z]\\d$",
                       residential_history_table$PostalCode %||% "")
  if (any(bad_pc_fmt)) {
    bad_first <- residential_history_table$PostalCode[bad_pc_fmt][1]
    stop(postal_code_error(bad_first), call. = FALSE)
  }
  
  bad_res_start <- check_date_format(residential_history_table$StartDate)
  bad_res_end   <- check_date_format(residential_history_table$EndDate)
  bad_exp_start <- check_date_format(exposure_window_start)
  bad_exp_end   <- check_date_format(exposure_window_end)
  
  if (bad_res_start + bad_res_end > 0)
    stop(sprintf(
      "Unable to process data. The StartDate and/or EndDate column(s) in the residential history file have %d date(s) that are incorrectly formatted or missing. Please ensure all dates are formatted as follows: YYYY-MM-DD.",
      bad_res_start + bad_res_end
    ), call. = FALSE)
  
  if (bad_exp_start + bad_exp_end > 0)
    stop(sprintf(
      "Unable to process data. The ExposureStartDate and/or ExposureEndDate column(s) in the exposure window file have %d date(s) that are incorrectly formatted or missing. Please ensure all dates are formatted as follows: YYYY-MM-DD.",
      bad_exp_start + bad_exp_end
    ), call. = FALSE)
  
  residential_history_table$StartDate <- as.Date(residential_history_table$StartDate)
  residential_history_table$EndDate   <- as.Date(residential_history_table$EndDate)
  exposure_window_start <- as.Date(exposure_window_start)
  exposure_window_end   <- as.Date(exposure_window_end)
  
  if (exposure_window_end < exposure_window_start) {
    stop(
      "Error: Unable to process individual. The provided end date for the exposure window occurs prior to the start date. Please ensure the end date occurs after the start date, and that the exposure window is at least 1 year in length.",
      call. = FALSE
    )
  }
  
  if (as.numeric(exposure_window_end - exposure_window_start + 1L) < 365) {
    stop(
      "Error: Unable to process individual. The provided exposure window is less than 1 year in length. Please ensure the exposure window is at least 1 year in length.",
      call. = FALSE
    )
  }
  
  # ---- 1. Prep residential history (merge duplicates, handle small gaps/overlaps)
  res_dt <- data.table::as.data.table(residential_history_table)[order(StartDate)]
  
  if (nrow(res_dt) > 1) {
    j <- 1L
    while (j < nrow(res_dt)) {
      if (res_dt$PostalCode[j] == res_dt$PostalCode[j + 1]) {
        res_dt$StartDate[j] <- min(res_dt$StartDate[j], res_dt$StartDate[j + 1])
        res_dt$EndDate[j]   <- max(res_dt$EndDate[j],   res_dt$EndDate[j + 1])
        res_dt <- res_dt[-(j + 1), ]
      } else {
        j <- j + 1L
      }
    }
  }
  
  if (any(res_dt$StartDate > res_dt$EndDate, na.rm = TRUE)) {
    pc <- res_dt$PostalCode[which(res_dt$StartDate > res_dt$EndDate)][1]
    stop(sprintf(
      "Error: Unable to process individual. In the residential history, the provided end date for postal code %s occurs prior to the start date. For all postal codes, please ensure the start date occurs before the end date, and that the residential history is contiguous and non-overlapping.",
      pc
    ), call. = FALSE)
  }
  
  if (nrow(res_dt) > 1) {
    for (i in seq_len(nrow(res_dt) - 1)) {
      prev_pc <- res_dt$PostalCode[i]
      next_pc <- res_dt$PostalCode[i + 1]
      s_next  <- res_dt$StartDate[i + 1]
      e_curr  <- res_dt$EndDate[i]
      if (s_next <= e_curr) {
        odays <- as.numeric(e_curr - s_next + 1)
        if (odays <= 90) {
          half <- ceiling(odays / 2)
          res_dt$EndDate[i]     <- s_next + half - 1
          res_dt$StartDate[i+1] <- s_next + half
          warning(sprintf(
            "Warning: Individual had a %d-day overlap in their residential history between %s and %s. To remove this overlap and ensure the residential history is contiguous, the first half of the overlapping period was assigned to %s and the second half assigned to %s.",
            odays, prev_pc, next_pc, prev_pc, next_pc
          ), call. = FALSE)
        } else {
          stop(sprintf(
            "Error: Individual had a %d-day overlap in their residential history between %s and %s. Because this overlap is greater than 90 days in length, the individual cannot be processed. Please ensure that the residential history is contiguous with no overlaps, or if there are overlaps, ensure that the overlaps are less than 90 days in length.",
            odays, prev_pc, next_pc
          ), call. = FALSE)
        }
      }
    }
  }
  
  if (nrow(res_dt) > 1) {
    for (i in seq_len(nrow(res_dt) - 1)) {
      prev_pc <- res_dt$PostalCode[i]
      next_pc <- res_dt$PostalCode[i + 1]
      gdays   <- as.numeric(res_dt$StartDate[i + 1] - res_dt$EndDate[i] - 1)
      if (gdays > 0 && gdays <= 90) {
        half <- floor(gdays / 2)
        res_dt$EndDate[i]     <- res_dt$EndDate[i] + half
        res_dt$StartDate[i+1] <- res_dt$StartDate[i+1] - (gdays - half)
        warning(sprintf(
          "Warning: Individual had a %d-day gap in their residential history between %s and %s. To fill this gap in residential history, the first half of gap was assigned to %s and the second half of the gap was assigned to %s.",
          gdays, prev_pc, next_pc, prev_pc, next_pc
        ), call. = FALSE)
      } else if (gdays > 90) {
        stop(sprintf(
          "Error: Individual had a %d-day gap in their residential history between %s and %s. Because this gap is greater than 90 days in length, the individual cannot be processed. Please ensure there are no gaps in the residential history, or if there are gaps, ensure that the gaps are less than 90 days in length.",
          gdays, prev_pc, next_pc
        ), call. = FALSE)
      }
    }
  }
  
  # ---- 2. Enforce data availability window (2010-01-01 to 2023-12-31) -------
  data_start <- as.Date("2010-01-01")
  data_end   <- as.Date("2023-12-31")
  orig_start <- exposure_window_start
  orig_end   <- exposure_window_end
  
  if (exposure_window_start < data_start) {
    exposure_window_start <- data_start
    warning(sprintf(
      "Warning: The provided start date (%s) of the exposure window is outside the data availability window %s. The exposure window start date was automatically set to January 1, 2010.",
      format(orig_start), sprintf("%s to %s", format(data_start), format(data_end))
    ), call. = FALSE)
  }
  if (exposure_window_end > data_end) {
    exposure_window_end <- data_end
    warning(sprintf(
      "Warning: The provided end date (%s) of the exposure window is outside the data availability window %s. The exposure window end date was automatically set to December 31, 2023.",
      format(orig_end), sprintf("%s to %s", format(data_start), format(data_end))
    ), call. = FALSE)
  }
  
  # ---- 3. Align exposure window to residential history -----------------------
  res_min <- min(res_dt$StartDate, na.rm = TRUE)
  res_max <- max(res_dt$EndDate,   na.rm = TRUE)
  orig_start2 <- exposure_window_start
  orig_end2   <- exposure_window_end
  
  if (exposure_window_start < res_min) {
    exposure_window_start <- res_min
    warning(sprintf(
      "Warning: The start date (%s) of the exposure window occurs outside the range of dates provided in the residential history file. The start date of the exposure window was automatically set to %s to ensure alignment between the exposure window and residential history.",
      format(orig_start2), format(exposure_window_start)
    ), call. = FALSE)
  }
  
  if (exposure_window_end > res_max) {
    exposure_window_end <- res_max
    warning(sprintf(
      "Warning: The end date (%s) of the exposure window occurs outside the range of dates provided in the residential history file. The end date of the exposure window was automatically set to %s to ensure alignment between the exposure window and residential history.",
      format(orig_end2), format(exposure_window_end)
    ), call. = FALSE)
  }
  
  if (as.numeric(exposure_window_end - exposure_window_start + 1L) < 365) {
    stop(
      "Error: Unable to process individual. The provided exposure window is less than 1 year in length. Please ensure the exposure window is at least 1 year in length.",
      call. = FALSE
    )
  }
  
  # ---- 4. Build partial-week table and aggregate to weekly_exposure ----------
  pw_list <- list(); ctr <- 1L
  
  for (i in seq_len(nrow(res_dt))) {
    pc       <- res_dt$PostalCode[i]
    pc_start <- res_dt$StartDate[i]
    pc_end   <- res_dt$EndDate[i]
    
    overlap_start <- max(pc_start, exposure_window_start)
    overlap_end   <- min(pc_end,   exposure_window_end)
    if (overlap_end < overlap_start) next
    
    dt <- fn_get_postal_code_data(pc, token_path = dropbox_token_file)
    
    dt[, c("epiweek_start_date","epiweek_end_date") :=
         lapply(.SD, as.Date), .SDcols = c("epiweek_start_date","epiweek_end_date")]
    
    dt <- dt[epiweek_end_date >= overlap_start & epiweek_start_date <= overlap_end]
    if (nrow(dt) == 0) next
    
    dt[, `:=`(
      partial_interval_start = pmax(epiweek_start_date, overlap_start),
      partial_interval_end   = pmin(epiweek_end_date,   overlap_end)
    )]
    dt[, days_overlap     := as.numeric(partial_interval_end - partial_interval_start) + 1L]
    dt[, days_overlap     := ifelse(days_overlap < 7, days_overlap, n_days)]
    dt[, fraction_of_week := days_overlap / n_days]
    dt[, partial_pm25_weekly_sum := pm25_weekly_sum * fraction_of_week]
    dt[, partial_n_days          := n_days * fraction_of_week]
    
    pw_list[[ctr]] <- dt[, .(
      epiweek_index, epiweek_start_date, epiweek_end_date,
      partial_pm25_weekly_sum, pm25_weekly_avg,
      partial_n_days, PostalCode
    )]
    ctr <- ctr + 1L
  }
  
  if (ctr == 1L) stop("No overlapping data found for this individual's history.", call. = FALSE)
  
  combine_partial <- data.table::rbindlist(pw_list, fill = TRUE)
  
  weekly_exposure <- combine_partial[, .(
    weekly_pm25_sum_i = sum(partial_pm25_weekly_sum, na.rm = TRUE),
    n_days            = sum(partial_n_days,          na.rm = TRUE),
    PostalCode        = PostalCode[which.max(partial_n_days)]
  ), by = .(epiweek_index, epiweek_start_date, epiweek_end_date)][order(epiweek_start_date)]
  
  weekly_exposure[, weekly_pm25_avg_i := weekly_pm25_sum_i / n_days]
  weekly_exposure <- weekly_exposure[n_days > 0]
  
  if (nrow(weekly_exposure) > 0 && weekly_exposure$n_days[1] < 7)
    weekly_exposure <- weekly_exposure[-1]
  if (nrow(weekly_exposure) > 0 && weekly_exposure$n_days[nrow(weekly_exposure)] < 7)
    weekly_exposure <- weekly_exposure[-nrow(weekly_exposure)]
  
  if (nrow(weekly_exposure) > 0) {
    weekly_exposure[, week_sequence := .I]
  } else {
    warning("Warning: No overlapping data found for this individual's history.", call. = FALSE)
    weekly_exposure[, week_sequence := integer()]
  }
  
  return(weekly_exposure[])
}

# (Token reading is handled within helpers; no global read check here)

#### 3  Install & load required R packages ####################################
required_packages <- c(
  "data.table", "dplyr", "tidyr", "lubridate", "purrr", "rdrop2",
  "httr", "ggplot2", "ggtext", "gridExtra", "patchwork", "tibble", "gt",
  "stringi", "readxl"
)
new_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(new_packages)) install.packages(new_packages)
lapply(required_packages, library, character.only = TRUE)
httr::set_config(httr::timeout(960))

`%||%` <- function(x, y) if (is.null(x)) y else x

#### 4  Province prefix -> abbreviation lookup ################################
province_lookup <- data.frame(
  start_pc = c('A','B','C','E','G','H','J','K','L','M','N','P','R','S','T','V','X','Y'),
  province = c(
    'Newfoundland','NovaScotia','PrinceEdwardIsland','NewBrunswick',
    'Quebec','Quebec','Quebec','Ontario','Ontario','Ontario','Ontario',
    'Ontario','Manitoba','Saskatchewan','Alberta','BritishColumbia',
    'NorthwestTerritories-Nunavut','Yukon'
  ),
  stringsAsFactors = FALSE
)
province_abbr <- c(
  Newfoundland = "NL", NovaScotia = "NS", PrinceEdwardIsland = "PE",
  NewBrunswick = "NB", Quebec = "QC", Ontario = "ON", Manitoba = "MB",
  Saskatchewan = "SK", Alberta = "AB", BritishColumbia = "BC",
  "NorthwestTerritories-Nunavut" = "NT", Yukon = "YT"
)

#### 5  Fetch & load postal-code file from Dropbox ############################
fn_get_postal_code_data <- function(postal_code,
                                    token_path  = resolve_token_path(),
                                    local_dir   = tempdir(),
                                    overwrite   = TRUE,
                                    timeout_sec = 960) {
  
  if (!dir.exists(local_dir)) dir.create(local_dir, recursive = TRUE)
  if (is.na(token_path) || !file.exists(token_path))
    stop("We ran into issues when trying to access the data on Dropbox. Please contact us to let us know this issue occurred and we will do our best to address the issue as soon as possible.",
         call. = FALSE)
  
  token <- readRDS(token_path)
  if (!any(c("Token2.0","Token","droptoken") %in% class(token)))
    stop("We ran into issues when trying to access the data on Dropbox. Please contact us to let us know this issue occurred and we will do our best to address the issue as soon as possible.",
         call. = FALSE)
  
  httr::set_config(httr::timeout(timeout_sec))
  
  prefix <- toupper(substr(postal_code, 1, 1))
  info   <- subset(province_lookup, start_pc == prefix)
  if (!nrow(info)) stop(postal_code_error(postal_code), call. = FALSE)
  province_folder <- info$province
  abbr <- province_abbr[[province_folder]]
  if (is.null(abbr)) stop(postal_code_error(postal_code), call. = FALSE)
  
  filename     <- paste0(abbr, "_PC_CanOSSEM_weekly_pm25_", postal_code, "_all_yrs.RDS")
  dropbox_path <- file.path("CanOSSEM_PostalCode_Weekly_2010-2023", province_folder, filename)
  local_file   <- file.path(local_dir, filename)
  
  if (!file.exists(local_file) || overwrite) {
    message("Downloading from Dropbox: ", dropbox_path)
    ok <- tryCatch({
      rdrop2::drop_download(dropbox_path, local_file, dtoken = token, overwrite = TRUE)
      TRUE
    }, error = function(e) {
      FALSE
    })
    if (!ok) stop(postal_code_error(postal_code), call. = FALSE)
  }
  
  message("Reading data from: ", local_file)
  dt <- data.table::as.data.table(readRDS(local_file))
  dt[, PostalCode := postal_code]
  
  if (!"epiweek_index" %in% names(dt)) {
    if ("epiweek" %in% names(dt)) data.table::setnames(dt, "epiweek", "epiweek_index")
    else stop("File lacks epiweek index", call. = FALSE)
  }
  
  dt <- dt[, .(
    pm25_weekly_sum = sum(pm25_weekly_sum, na.rm = TRUE),
    pm25_weekly_avg = sum(pm25_weekly_avg * n_days, na.rm = TRUE) / sum(n_days, na.rm = TRUE),
    n_days          = sum(n_days, na.rm = TRUE),
    PostalCode      = first(PostalCode)
  ), by = .(epiweek_index, epiweek_start_date, epiweek_end_date)][]
  dt
}

#### 6  Modified-Z helper ######################################################
fn_calc_modified_z <- function(x) {
  med  <- median(x, na.rm = TRUE)
  madv <- mad(x, constant = 1.4826, na.rm = TRUE)
  if (madv == 0) return(rep(NA_real_, length(x)))
  (x - med) / madv
}

#### 7  Counterfactual trajectory builder #####################################
fn_build_counterfactual_trajectory <- function(df) {
  df <- data.table::copy(df)[order(week_sequence)]
  
  df[, cumulative_total_pm25_sum_individual := cumsum(weekly_pm25_sum_i)]
  df[, modified_z := fn_calc_modified_z(weekly_pm25_sum_i)]
  df[, start_date := epiweek_start_date]
  
  df[, wildfire_season :=
       (lubridate::month(epiweek_end_date)   >= 5 &
          lubridate::month(epiweek_start_date) <= 10)]
  
  counterfactual_slope <- df[
    !is.na(modified_z) & modified_z >= -2 & modified_z <= 2 & n_days == 7,
    median(weekly_pm25_sum_i, na.rm = TRUE)
  ]
  if (!is.finite(counterfactual_slope)) counterfactual_slope <- 0
  
  df[, weekly_non_wfs_pm25_sum_i := ifelse(
    modified_z > 2 & wildfire_season,
    counterfactual_slope,
    weekly_pm25_sum_i
  )]
  
  df[, cumulative_non_wfs_pm25_sum_individual := cumsum(weekly_non_wfs_pm25_sum_i)]
  df[, weekly_wfs_pm25_sum_i := weekly_pm25_sum_i - weekly_non_wfs_pm25_sum_i]
  df[, cumulative_wfs_pm25_sum_individual := cumsum(weekly_wfs_pm25_sum_i)]
  df[, weekly_non_wfs_pm25_avg_i := weekly_non_wfs_pm25_sum_i / n_days]
  df[, weekly_wfs_pm25_avg_i     := weekly_wfs_pm25_sum_i     / n_days]
  
  df[]
}

#### 8  Identify smoke episodes ###############################################
fn_identify_smoke_episodes <- function(is_smoke_impacted, cont_week,
                                       weekly_sum_wfs_pm25_values,
                                       episode_threshold = 0,
                                       num_weeks_in_episode = 2,
                                       max_no_smoke_gap = 3,
                                       weeks_needed_above_threshold = 1) {
  
  n <- length(is_smoke_impacted); epi_id <- integer(n)
  cur_id <- 0; in_epi <- FALSE; smoke_wks <- wks_above <- gap <- 0
  bridge <- integer(0); fail <- function(id) epi_id[epi_id == id] <<- 0
  
  for (i in seq_len(n)) {
    if (i > 1 && cont_week[i] - cont_week[i-1] > 1) {
      if (in_epi && (smoke_wks < num_weeks_in_episode ||
                     wks_above < weeks_needed_above_threshold)) fail(cur_id)
      in_epi <- FALSE; smoke_wks <- wks_above <- gap <- 0; bridge <- integer(0)
    }
    
    if (is_smoke_impacted[i] == 1) {
      if (!in_epi) { cur_id <- cur_id + 1; in_epi <- TRUE; smoke_wks <- wks_above <- 0 }
      if (length(bridge) > 0) { epi_id[bridge] <- cur_id; bridge <- integer(0) }
      smoke_wks <- smoke_wks + 1
      if (weekly_sum_wfs_pm25_values[i] > episode_threshold) wks_above <- wks_above + 1
      epi_id[i] <- cur_id; gap <- 0
      
    } else if (in_epi) {
      gap <- gap + 1; bridge <- c(bridge, i)
      if (gap > max_no_smoke_gap) {
        if (smoke_wks < num_weeks_in_episode ||
            wks_above < weeks_needed_above_threshold) fail(cur_id)
        in_epi <- FALSE; bridge <- integer(0)
      }
    }
  }
  
  if (in_epi && (smoke_wks < num_weeks_in_episode ||
                 wks_above < weeks_needed_above_threshold)) fail(cur_id)
  
  for (id in setdiff(unique(epi_id), 0)) {
    idx       <- which(epi_id == id)
    smoke_idx <- idx[is_smoke_impacted[idx] == 1]
    epi_id[idx[idx > max(smoke_idx)]] <- 0
  }
  uniq <- setdiff(unique(epi_id), 0)
  for (k in seq_along(uniq)) epi_id[epi_id == uniq[k]] <- k
  
  epi_id
}

#### 9  Compute metrics ########################################################
fn_compute_metrics <- function(df,
                               exposure_start,
                               exposure_end,
                               smoke_week_threshold = 0) {
  
  MICRO_TO_MILLI <- 1 / 1000
  df <- df %>% dplyr::mutate(is_smoke_impacted = as.integer(weekly_wfs_pm25_avg_i > smoke_week_threshold))
  
  total_smoke_weeks <- sum(df$is_smoke_impacted, na.rm = TRUE)
  mean_smoke_week   <- if (total_smoke_weeks > 0)
    mean(df$weekly_wfs_pm25_avg_i[df$is_smoke_impacted == 1], na.rm = TRUE) else 0
  
  weeks_wfs_over_5         <- sum(df$weekly_wfs_pm25_avg_i > 5,  na.rm = TRUE)
  weeks_total_pm25_over_25 <- sum(df$weekly_wfs_pm25_avg_i > 0 & df$weekly_pm25_avg_i > 25, na.rm = TRUE)
  
  num_normal_episodes <- max(df$episode_id,        na.rm = TRUE)
  num_severe_episodes <- max(df$severe_episode_id, na.rm = TRUE)
  
  epi_summary_span <- df %>%
    dplyr::filter(episode_id > 0) %>%
    dplyr::group_by(episode_id) %>%
    dplyr::summarise(span_weeks = max(week_sequence) - min(week_sequence) + 1,
                     .groups = "drop")
  
  longest_episode_len <- if (nrow(epi_summary_span) > 0)
    max(epi_summary_span$span_weeks, na.rm = TRUE) else 0
  
  epi_df <- df %>% dplyr::filter(episode_id > 0, is_smoke_impacted == 1)
  episode_summary <- epi_df %>%
    dplyr::group_by(episode_id) %>%
    dplyr::summarise(avg_pm25 = mean(weekly_wfs_pm25_avg_i, na.rm = TRUE),
                     .groups = "drop")
  
  worst_episode_exposure <- if (nrow(episode_summary) > 0)
    max(episode_summary$avg_pm25, na.rm = TRUE) else 0
  
  severe_pm25_cum <- df %>% dplyr::filter(severe_episode_id > 0, is_smoke_impacted == 1) %>%
    dplyr::summarise(sum(weekly_wfs_pm25_sum_i, na.rm = TRUE)) %>% dplyr::pull()
  
  obs_cumul_mg <- dplyr::last(df$cumulative_total_pm25_sum_individual) * MICRO_TO_MILLI
  wfs_cumul_mg <- dplyr::last(df$cumulative_wfs_pm25_sum_individual)   * MICRO_TO_MILLI
  cf_cumul_mg  <- sum(df$weekly_non_wfs_pm25_sum_i, na.rm = TRUE) * MICRO_TO_MILLI
  
  pct_wfs_from_severe <- if (wfs_cumul_mg > 0)
    100 * severe_pm25_cum * MICRO_TO_MILLI / wfs_cumul_mg else NA_real_
  
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
  
  numbered <- tibble::tibble(
    `1_Cumulative_WFS_PM25`        = round(wfs_cumul_mg, 2),
    `2_WFS_Fraction`               = round(100 * wfs_cumul_mg / obs_cumul_mg, 2),
    `3_Average_WFS_PM25`           = round(mean_smoke_week, 2),
    `4_Any_WFS`                    = total_smoke_weeks,
    `5_WFS_PM25_exceeds_5`         = weeks_wfs_over_5,
    `6_Total_PM25_exceeds_25`      = weeks_total_pm25_over_25,
    `7_WFS_Episodes`               = num_normal_episodes,
    `8_Severe_Episodes`            = num_severe_episodes,
    `9_Longest_Episode`            = longest_episode_len,
    `10_Worst_Episode`             = round(worst_episode_exposure, 2),
    `11_WFS_from_Severe_Episodes`  = round(pct_wfs_from_severe, 1),
    `12_Average_Recovery`          = avg_time_between
  )
  
  extras <- tibble::tibble(
    `Cumulative_Total_PM25`     = round(obs_cumul_mg, 2),
    `Average_Total_PM25`        = round(mean(df$weekly_pm25_avg_i, na.rm = TRUE), 2),
    `Cumulative_NonWFS_PM25`    = round(cf_cumul_mg, 2),
    `Average_NonWFS_PM25`       = round(mean(df$weekly_non_wfs_pm25_avg_i, na.rm = TRUE), 2),
    `Counterfactual_Value`      = round(unique(df$weekly_non_wfs_pm25_avg_i[df$is_smoke_impacted == 1]), 2)
  )
  
  dplyr::bind_cols(numbered, extras)
}

#### 10  Plotting functions ####################################################
median_range_color        <- "#B0C6DF"
wildfire_attributable_col <- "#FF8572"

fn_plot_cumulative_sum_by_year <- function(df, y_limits = NULL, line_thin = PLOT_LINE_THIN) {
  df_plot <- df %>% dplyr::mutate(
    total   = cumulative_total_pm25_sum_individual / 1000,
    non_wfs = cumulative_non_wfs_pm25_sum_individual / 1000,
    wfs     = cumulative_wfs_pm25_sum_individual     / 1000
  )
  y_max <- if (is.null(y_limits))
    max(c(df_plot$total, df_plot$non_wfs, df_plot$wfs), na.rm = TRUE) * 1.20
  else y_limits[2]
  
  ggplot2::ggplot(df_plot, ggplot2::aes(x = start_date)) +
    ggplot2::geom_line(ggplot2::aes(y = total,   color = "Total"),   linewidth = 1) +
    ggplot2::geom_line(ggplot2::aes(y = non_wfs, color = "Non-WFS"), linewidth = 1) +
    ggplot2::geom_line(ggplot2::aes(y = wfs,     color = "WFS"),     linewidth = 1) +
    ggplot2::scale_color_manual(values = c(
      "Total"   = "black",
      "Non-WFS" = median_range_color,
      "WFS"     = wildfire_attributable_col
    ), breaks = c("Total","Non-WFS","WFS"), name = NULL) +
    ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, y_max)) +
    ggplot2::labs(title = "A", x = "Year",
                  y = expression("Cumulative PM"[2.5]*" (mg/m"^3*")")) +
    ggplot2::theme_classic(base_size = PLOT_BASE_SIZE) +
    ggplot2::theme(
      legend.position = "top",
      axis.line  = ggplot2::element_line(linewidth = PLOT_LINE_THIN),
      axis.ticks = ggplot2::element_line(linewidth = PLOT_LINE_THIN),
      plot.title = ggplot2::element_text(hjust = 0)
    )
}

fn_plot_time_series_weekly_mean <- function(df, y_limits = NULL,
                                            line_thin = PLOT_LINE_THIN,
                                            alpha_norm = 0.15,
                                            alpha_severe = 0.20,
                                            fill_norm = "orange",
                                            fill_severe = "red3") {
  
  df_plot <- df %>% dplyr::mutate(
    wfs     = dplyr::coalesce(weekly_wfs_pm25_avg_i,     0),
    non_wfs = dplyr::coalesce(weekly_non_wfs_pm25_avg_i, 0),
    total   = dplyr::coalesce(weekly_pm25_avg_i,         0)
  )
  
  epi_spans <- df_plot %>%
    dplyr::filter(episode_id > 0) %>%
    dplyr::group_by(episode_id) %>%
    dplyr::summarise(start = min(start_date,       na.rm = TRUE),
                     end   = max(epiweek_end_date, na.rm = TRUE),
                     .groups = "drop")
  
  sev_spans <- df_plot %>%
    dplyr::filter(severe_episode_id > 0) %>%
    dplyr::group_by(severe_episode_id) %>%
    dplyr::summarise(start = min(start_date,       na.rm = TRUE),
                     end   = max(epiweek_end_date, na.rm = TRUE),
                     .groups = "drop")
  
  y_max <- if (is.null(y_limits))
    max(c(df_plot$total, df_plot$non_wfs, df_plot$wfs), na.rm = TRUE) * 1.20
  else y_limits[2]
  
  ggplot2::ggplot() +
    ggplot2::geom_rect(data = epi_spans, inherit.aes = FALSE,
                       ggplot2::aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf,
                                    fill = "Episode"), alpha = alpha_norm) +
    ggplot2::geom_rect(data = sev_spans, inherit.aes = FALSE,
                       ggplot2::aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf,
                                    fill = "Severe episode"), alpha = alpha_severe) +
    ggplot2::geom_line(data = df_plot,
                       ggplot2::aes(x = start_date, y = total,   color = "Total"),   linewidth = 1) +
    ggplot2::geom_line(data = df_plot,
                       ggplot2::aes(x = start_date, y = non_wfs, color = "Non-WFS"), linewidth = 1) +
    ggplot2::geom_line(data = df_plot,
                       ggplot2::aes(x = start_date, y = wfs,     color = "WFS"),     linewidth = 1) +
    ggplot2::scale_fill_manual(values = c("Episode" = "orange",
                                          "Severe episode" = "red3"),
                               name = NULL,
                               guide = ggplot2::guide_legend(override.aes = list(alpha = 0.4))) +
    ggplot2::scale_color_manual(values = c(
      "Total"   = "black",
      "Non-WFS" = median_range_color,
      "WFS"     = wildfire_attributable_col
    ), breaks = c("Total","Non-WFS","WFS"),
    name   = NULL,
    guide  = ggplot2::guide_legend(override.aes = list(fill = NA, linewidth = 1.5))) +
    ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, y_max)) +
    ggplot2::labs(title = "B", x = "Year",
                  y = expression("Mean PM"[2.5]*" ("*mu*"g/m"^3*")")) +
    ggplot2::theme_classic(base_size = PLOT_BASE_SIZE) +
    ggplot2::theme(
      legend.position = "top",
      axis.line  = ggplot2::element_line(linewidth = line_thin),
      axis.ticks = ggplot2::element_line(linewidth = line_thin),
      plot.title = ggplot2::element_text(hjust = 0)
    )
}

fn_plot_histogram <- function(df, binwidth = 8, line_thin = PLOT_LINE_THIN) {
  slopes <- df$weekly_pm25_sum_i
  z_vals <- df$modified_z
  
  stable_idx <- which(!is.na(z_vals) & z_vals >= -2 & z_vals <= 2)
  min_slope  <- if (length(stable_idx)) min(slopes[stable_idx]) else NA_real_
  max_slope  <- if (length(stable_idx)) max(slopes[stable_idx]) else NA_real_
  
  tmp_hist <- ggplot2::ggplot_build(
    ggplot2::ggplot(df, ggplot2::aes(x = weekly_pm25_sum_i)) +
      ggplot2::geom_histogram(binwidth = binwidth, boundary = 0, na.rm = TRUE)
  )
  y_max_count <- max(tmp_hist$data[[1]]$count, na.rm = TRUE) * 1.10
  rect_df <- data.frame(xmin = min_slope, xmax = max_slope,
                        ymin = 0, ymax = y_max_count)
  
  x_max <- max(slopes, na.rm = TRUE) * 1.05
  
  ggplot2::ggplot() +
    ggplot2::geom_rect(data = rect_df,
                       ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                                    fill = "-2 < Z-Score < 2"), alpha = 0.3) +
    ggplot2::geom_histogram(data = df,
                            ggplot2::aes(x = weekly_pm25_sum_i),
                            binwidth = binwidth, boundary = 0,
                            fill = "grey90", color = "black",
                            linewidth = line_thin, show.legend = FALSE,
                            na.rm = TRUE) +
    { if (!is.na(min_slope))
      ggplot2::geom_vline(xintercept = min_slope, linetype = "dashed",
                          linewidth = line_thin) } +
    { if (!is.na(max_slope))
      ggplot2::geom_vline(xintercept = max_slope, linetype = "dashed",
                          linewidth = line_thin) } +
    ggplot2::geom_vline(ggplot2::aes(xintercept = median(slopes, na.rm = TRUE),
                                     color = "Median"), linewidth = 1.2) +
    ggplot2::scale_fill_manual(values = c("-2 < Z-Score < 2" = "yellow"), name = NULL) +
    ggplot2::scale_color_manual(values = c("Median" = "#0085E4"), name = NULL) +
    ggplot2::guides(fill = ggplot2::guide_legend(order = 1),
                    color = ggplot2::guide_legend(order = 2)) +
    ggplot2::scale_x_continuous(limits = c(0, x_max), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(0, y_max_count), expand = c(0, 0)) +
    ggplot2::labs(title = "C",
                  x = expression("Weekly Sum of PM"[2.5]*" ("*mu*"g/m"^3*" per epi-week)"),
                  y = "Count") +
    ggplot2::theme_classic(base_size = PLOT_BASE_SIZE) +
    ggplot2::theme(
      legend.position = "top",
      axis.line  = ggplot2::element_line(linewidth = line_thin),
      axis.ticks = ggplot2::element_line(linewidth = line_thin),
      plot.title = ggplot2::element_text(hjust = 0),
      panel.grid = ggplot2::element_blank()
    )
}

#### 11  Run individual analysis ##############################################
fn_run_individual_analysis <- function(res_hist,
                                       start_exposure,
                                       end_exposure,
                                       token_path,
                                       IndividualID) {
  
  weekly_exposure <- fn_build_individual_exposure_history(
    residential_history_table = res_hist,
    exposure_window_start     = start_exposure,
    exposure_window_end       = end_exposure,
    dropbox_token_file        = token_path
  )
  
  trajectory <- fn_build_counterfactual_trajectory(weekly_exposure)
  
  trajectory[, episode_id := fn_identify_smoke_episodes(
    is_smoke_impacted          = as.integer(weekly_wfs_pm25_avg_i > 0),
    cont_week                  = week_sequence,
    weekly_sum_wfs_pm25_values = weekly_wfs_pm25_sum_i,
    episode_threshold          = 0,
    num_weeks_in_episode       = 2,
    max_no_smoke_gap           = 3,
    weeks_needed_above_threshold = 1
  )]
  
  trajectory[, severe_episode_id := fn_identify_smoke_episodes(
    is_smoke_impacted          = as.integer(weekly_wfs_pm25_avg_i > 0),
    cont_week                  = week_sequence,
    weekly_sum_wfs_pm25_values = weekly_wfs_pm25_sum_i,
    episode_threshold          = 250,
    num_weeks_in_episode       = 1,
    max_no_smoke_gap           = 3,
    weeks_needed_above_threshold = 1
  )]
  
  # Combine normal + severe into contiguous episode blocks for visuals
  episodes_combined <- rep(0L, nrow(trajectory))
  epi_counter <- 1L
  for (i in seq_len(nrow(trajectory))) {
    curr_has_epi <- trajectory$episode_id[i] > 0L ||
      trajectory$severe_episode_id[i] > 0L
    next_has_epi <- if (i < nrow(trajectory))
      (trajectory$episode_id[i+1] > 0L ||
         trajectory$severe_episode_id[i+1] > 0L) else FALSE
    if (curr_has_epi) {
      episodes_combined[i] <- epi_counter
      if (!next_has_epi) epi_counter <- 1 + epi_counter
    }
  }
  trajectory[, episode_id := episodes_combined]
  
  metrics <- fn_compute_metrics(
    trajectory,
    exposure_start = start_exposure,
    exposure_end   = end_exposure
  )
  
  # Defer figure generation; return data & metrics only
  list(id = IndividualID,
       metrics = metrics,
       data = trajectory)
}

#### 11.5  Single-ID runner (handles repeated/duplicate windows) ##############
fn_single <- function(d, token_path) {
  id <- unique(d$IndividualID)
  
  exp_rows_raw <- d[, .(ExposureStartDate, ExposureEndDate)]
  exp_rows_raw <- exp_rows_raw[!is.na(ExposureStartDate) & !is.na(ExposureEndDate)]
  if (nrow(exp_rows_raw) == 0) {
    stop(sprintf("Missing exposure window for ID %s. Ensure ExposureStartDate/ExposureEndDate are provided.", id), call. = FALSE)
  }
  
  res_rows_n <- nrow(unique(d[, .(PostalCode, StartDate, EndDate)]))
  exp_unique <- unique(exp_rows_raw)
  multiple_distinct <- nrow(exp_unique) > 1L
  identical_dups <- (nrow(exp_unique) == 1L) && res_rows_n > 0L &&
    (nrow(exp_rows_raw) > res_rows_n)
  
  if (multiple_distinct || identical_dups) {
    warning("Warning: Multiple exposure windows were provided for this individual. Data were processed using the first available exposure window.", call. = FALSE)
  }
  
  win <- exp_unique[order(ExposureStartDate, ExposureEndDate)][1, ]
  
  fn_run_individual_analysis(
    res_hist       = d[, .(PostalCode, StartDate, EndDate)],
    start_exposure = win$ExposureStartDate,
    end_exposure   = win$ExposureEndDate,
    token_path     = token_path,
    IndividualID   = id
  )
}

#### 12  Batch-processing wrapper #############################################
fn_run_batch_analysis <- function(history_df,
                                  token_path   = resolve_token_path(),
                                  parallel     = FALSE,
                                  n_cores      = max(1, parallel::detectCores() - 1),
                                  out_dir_root = getwd()) {
  
  if (!is.character(token_path) || length(token_path) != 1 || !file.exists(token_path))
    stop("We ran into issues when trying to access the data on Dropbox. Please contact us to let us know this issue occurred and we will do our best to address the issue as soon as possible.",
         call. = FALSE)
  
  req_cols <- c("IndividualID", "PostalCode", "StartDate", "EndDate",
                "ExposureStartDate", "ExposureEndDate")
  if (!all(req_cols %in% names(history_df)))
    stop(sprintf("history_df must contain the following columns: %s",
                 paste(req_cols, collapse = ", ")), call. = FALSE)
  
  res_ids <- unique(history_df$IndividualID[!is.na(history_df$PostalCode)])
  exp_ids <- unique(history_df$IndividualID[!is.na(history_df$ExposureStartDate)])
  
  missing_in_exp <- setdiff(res_ids, exp_ids)
  if (length(missing_in_exp)) {
    stop(sprintf(
      "Unable to process data. There is a mismatch between the IndividualIDs in the residential history and exposure window files. The following IDs are included in the residential history file but not in the exposure window file: %s. Please ensure that the files have the same number of IndividualIDs, and that each IndividualID exists in both files.",
      paste(missing_in_exp, collapse = ", ")
    ), call. = FALSE)
  }
  
  missing_in_res <- setdiff(exp_ids, res_ids)
  if (length(missing_in_res)) {
    stop(sprintf(
      "Unable to process data. There is a mismatch between the IndividualIDs in the exposure window file and the residential history file. The following IDs are included in the exposure window file but not in the residential history file: %s. Please ensure that the files have the same number of IndividualIDs, and that each IndividualID exists in both files.",
      paste(missing_in_res, collapse = ", ")
    ), call. = FALSE)
  }
  
  dt <- data.table::as.data.table(history_df)
  data.table::setorder(dt, IndividualID, StartDate)
  by_id <- split(dt, dt$IndividualID)
  
  log_df   <- data.table::data.table(IndividualID = character(),
                                     Message = character())
  out_list <- list()
  
  for (id in names(by_id)) {
    d <- by_id[[id]]
    
    warnings_collected <- character()
    errors_collected   <- character()
    res <- NULL
    
    res <- tryCatch(
      withCallingHandlers(
        fn_single(d, token_path = token_path),
        warning = function(w) {
          m <- conditionMessage(w)
          if (!grepl("warnings? in .*summarise\\(", m, ignore.case = TRUE)) {
            warnings_collected <<- c(warnings_collected, m)
          }
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) {
        errors_collected <<- c(errors_collected, conditionMessage(e))
        NULL
      }
    )
    
    msg_parts <- c(warnings_collected, errors_collected)
    if (length(msg_parts) == 0) msg_parts <- "No issues"
    msg <- paste(msg_parts, collapse = "; ")
    
    log_df <- rbind(log_df, data.table::data.table(IndividualID = id, Message = msg))
    if (!is.null(res)) out_list[[id]] <- res
  }
  
  data.table::fwrite(log_df, file = file.path(out_dir_root, "Warning_Error_Log.csv"))
  message(sprintf(
    "Processed %d IDs; %d had issues. Please see Warning_Error_Log.csv.",
    nrow(log_df), sum(log_df$Message != "No issues")
  ))
  
  if (length(out_list) > 0) {
    metrics_wide <- data.table::rbindlist(
      lapply(names(out_list), function(id) {
        df <- out_list[[id]]$metrics
        df$IndividualID <- id
        data.table::as.data.table(df)[, c("IndividualID",
                                          setdiff(names(df), "IndividualID")), with = FALSE]
      }), use.names = TRUE, fill = TRUE
    )
  } else {
    metrics_wide <- data.table::data.table()
  }
  
  # Defer figure generation; build plots on demand downstream
  plots_list <- list()
  data_list  <- setNames(lapply(out_list, `[[`, "data"), names(out_list))
  
  batch <- list(metrics_wide = metrics_wide,
                plots        = plots_list,
                data         = data_list)
  
  if (length(out_list) == 0) return(invisible(NULL))
  
  save_batch_outputs(batch, out_dir_root)
  invisible(batch)
}

#### 13  Output helpers ########################################################
sanitize_names <- function(x) {
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
  # Ensure output dir exists
  dir.create(out_dir_root, recursive = TRUE, showWarnings = FALSE)
  
  # ---- 1) Metrics CSV -------------------------------------------------------
  metrics_csv <- data.table::copy(batch$metrics_wide)
  data.table::setnames(metrics_csv, sanitize_names(names(metrics_csv)))
  data.table::fwrite(metrics_csv, file = file.path(out_dir_root, "MultiWiSE_Metrics.csv"))
  
  # ---- 2) Per-ID plots (create folder ONLY if we actually save plots) -------
  if (length(batch$plots)) {
    plots_dir <- file.path(out_dir_root, "plots")
    dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)
    invisible(lapply(names(batch$plots), function(id) {
      ggplot2::ggsave(
        filename = file.path(plots_dir, sprintf("%s.png", id)),
        plot     = batch$plots[[id]],
        width    = 14, height = 28, dpi = 300
      )
    }))
  }
  
  # ---- 3) Weekly PM2.5 long table ------------------------------------------
  long_weekly <- data.table::rbindlist(batch$data, idcol = "IndividualID", use.names = TRUE)
  long_weekly[, epiweek_index_rel := seq_len(.N), by = IndividualID]
  
  rename_map <- c(
    "weekly_pm25_avg_i"         = "weekly_pm25_avg",
    "weekly_wfs_pm25_avg_i"     = "weekly_wfs_pm25_avg",
    "weekly_non_wfs_pm25_avg_i" = "weekly_non_wfs_pm25_avg",
    "weekly_pm25_sum_i"         = "weekly_pm25_sum",
    "weekly_wfs_pm25_sum_i"     = "weekly_wfs_pm25_sum",
    "weekly_non_wfs_pm25_sum_i" = "weekly_non_wfs_pm25_sum",
    "epiweek_index_rel"         = "epiweek_index"
  )
  for (old in names(rename_map)) {
    if (old %in% names(long_weekly)) {
      data.table::setnames(long_weekly, old, rename_map[[old]])
    }
  }
  
  cols_keep <- c(
    "IndividualID", "PostalCode", "epiweek_index", "n_days",
    "epiweek_start_date", "epiweek_end_date",
    "weekly_pm25_avg", "weekly_wfs_pm25_avg", "weekly_non_wfs_pm25_avg",
    "weekly_pm25_sum", "weekly_wfs_pm25_sum", "weekly_non_wfs_pm25_sum",
    "episode_id", "severe_episode_id"
  )
  long_weekly <- long_weekly[, ..cols_keep]
  
  # Rounding for CSV
  avg_cols <- c("weekly_pm25_avg", "weekly_wfs_pm25_avg", "weekly_non_wfs_pm25_avg")
  sum_cols <- c("weekly_pm25_sum", "weekly_wfs_pm25_sum", "weekly_non_wfs_pm25_sum")
  for (cl in intersect(avg_cols, names(long_weekly))) long_weekly[[cl]] <- signif(as.numeric(long_weekly[[cl]]), 3)
  for (cl in intersect(sum_cols, names(long_weekly))) long_weekly[[cl]] <- signif(as.numeric(long_weekly[[cl]]), 4)
  
  data.table::fwrite(long_weekly, file = file.path(out_dir_root, "Weekly_PM25_Estimates.csv"))
  
}


#### User Interface (UI) ####

has_backend <- function() TRUE

# Cross-platform zip helper
zip_dir <- function(src_dir, zip_path) {
  files <- list.files(src_dir, all.files = FALSE, recursive = TRUE,
                      include.dirs = TRUE, no.. = TRUE)
  old <- setwd(src_dir); on.exit(setwd(old), add = TRUE)
  if (requireNamespace("zip", quietly = TRUE)) {
    zip::zipr(zipfile = zip_path, files = files)
  } else {
    utils::zip(zipfile = zip_path, files = files)
  }
}

# Shiny Theme (COSMO)
theme <- bs_theme(version = 5, bootswatch = "cosmo")
# Custom CSS tweaks
theme <- bslib::bs_add_rules(theme, "
  body { padding-top: 60px; }
  @media (max-width: 991.98px) { body { padding-top: 56px; } }
  .tab-content { padding-bottom: 3rem; }
  .sidebar .btn-sm { padding: .25rem .5rem; font-size: 1em; background-color: none; }
  .results-download-bar { display:flex; justify-content:flex-end; gap:.5rem; margin-bottom:10px; }

  .muted { opacity: .25 !important; filter: saturate(0.2); pointer-events: none !important; }

  .shiny-file-input .input-group > .btn { padding: 0.75rem 1rem !important; font-size: 1rem !important; }
  .shiny-file-input .input-group > .form-control { height: 2.75rem !important; font-size: 1rem !important; padding: .5rem .75rem !important; }
  .shiny-file-input .shiny-file-input-text { line-height: 1.75rem !important; }

  .shiny-file-input .progress, .shiny-file-input-progress .progress { --bs-progress-height: 1.8rem !important; height: 1.8rem !important; border-radius: 0 !important; }
  .shiny-file-input .progress .progress-bar, .shiny-file-input-progress .progress .progress-bar {
    height: 1.8rem !important; line-height: 1.8rem !important; font-size: 1.05rem !important; font-weight: 600; letter-spacing: 0.2px; color: #fff;
  }
  .shiny-file-input-progress .shiny-file-input-progress-text, .shiny-file-input-progress small, .shiny-file-input-progress .text-success, .shiny-file-input-progress .text-muted {
    font-size: 1rem !important;
  }

  .form-control, .btn, .input-group-text { font-size: 1rem; }
  .shiny-input-container > label { font-size: 1.05rem; }

  #profile_type .shiny-options-group { display: flex; gap: 18px; align-items: center; flex-wrap: wrap; }
  #profile_type .form-check-inline { margin-right: 18px; }
  #profile_type .form-check-inline:last-child { margin-right: 0; }

  #run_analysis.btn { font-size: 1rem; padding: 1rem 1rem; }

  #res_file_progress.progress, #exp_file_progress.progress {
    --bs-progress-height: 1.6rem !important; height: 1.6rem !important; border-radius: 0 !important; margin-top: 6px !important;
  }
  #res_file_progress .progress-bar, #exp_file_progress .progress-bar {
    height: 1.6rem !important; line-height: 1.6rem !important; font-size: 0.95rem !important; font-weight: 600 !important; letter-spacing: 0.2px !important; color: #fff !important; white-space: nowrap !important;
  }

  .upload-block { margin-bottom: 18px; }
  .upload-block .section-title { margin: 10px 0 2px; font-weight: 600; }
  .upload-block .req { display: block; margin: -2px 0 8px; color: #6c757d; font-size: .92rem; }
")

theme <- bslib::bs_add_rules(theme, "
  /* Make 'Upload Files' (sidebar h3) and 'Results' the same size */
  .bslib-sidebar h3,
  #results_hdr {
    font-size: 2rem;
    margin-top: 0;
    margin-bottom: 0;
  }
")

theme <- bslib::bs_add_rules(theme, "
  #warn_badge {
    display: none;
    font-weight: 400;
    width: 1.25rem; height: 1.25rem;
    border-radius: 50% !important;
    padding: 0;
    align-items: center; justify-content: center;
    line-height: 1; font-size: 0.85rem;
  }
")

theme <- bslib::bs_add_rules(theme, "
  .upload-block { margin-bottom: 0px; }
  .upload-block .section-title { margin: 0px 0 0; }
  .upload-block .req { margin: 0 0 0px; }
  .upload-block .shiny-input-container { margin-bottom: 2px !important; }
")
theme <- bslib::bs_add_rules(theme, "
  .upload-block .req { margin: 4px 0 8px !important; }
  .upload-block .req + .shiny-input-container { margin-top: 4px !important; }
")
theme <- bslib::bs_add_rules(theme, "
  #run_analysis.btn { font-size: 1.25rem !important; padding: 1rem 1.25rem !important; letter-spacing: .2px; }
  @media (min-width: 992px) { #run_analysis.btn { font-size: 1.35rem !important; } }
")

# Floating “preparing” indicator CSS (indicator disabled in server by design)
theme <- bslib::bs_add_rules(theme, "
  /* Floating prep indicator (bottom-right) */
  #prep_indicator {
    position: fixed;
    right: 18px;
    bottom: 18px;
    z-index: 1080;
    display: none; /* hidden by default */
  }
  #prep_indicator .card {
    border-radius: .75rem;
    padding: .75rem .9rem;
  }
  #prep_indicator .prep-row {
    display: flex; align-items: center; gap: .6rem;
  }
  #prep_indicator .prep-text {
    font-weight: 600; letter-spacing: .2px;
  }
")

# Inline error styling for upload size message under file choosers
theme <- bslib::bs_add_rules(theme, "
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
    margin-top: 0;
    margin-bottom: 0;
    line-height: 1.2;
  }

  /* Tighten spacing around both choosers & their inline error slots */
  #res_file, #exp_file { margin-bottom: 0 !important; }
  #res_file.shiny-input-container, #exp_file.shiny-input-container { margin-bottom: 0 !important; }
  #res_file .form-label, #exp_file .form-label { margin-bottom: 2px !important; }
  #res_file .input-group, #exp_file .input-group { margin-bottom: 0 !important; }
  #res_file .form-control, #exp_file .form-control { margin-bottom: 0 !important; }
  #res_size_error { margin-top: 0 !important; }
  #exp_size_error { margin-top: 0 !important; }
")

# Hide Shiny's default fileInput progress/error strip (we show inline text instead)
theme <- bslib::bs_add_rules(theme, "
  #res_file_progress { display: none !important; }
  #exp_file_progress { display: none !important; }
")

# Suppress the default red failure bar for fileInput progress (safeguard)
theme <- bslib::bs_add_rules(theme, "
  .shiny-file-input-progress .progress .progress-bar.bg-danger,
  .shiny-file-input-progress .progress .progress-bar.progress-bar-danger {
    display: none !important;
  }
")

required_cols_res <- c("IndividualID", "PostalCode", "StartDate", "EndDate")
required_cols_exp <- c("IndividualID", "ExposureStartDate", "ExposureEndDate")

ui <- navbarPage(
  title = HTML("Multi<b>WiSE</b> Canada"),
  theme = theme,
  collapsible = TRUE,
  windowTitle = "MultiWiSE Individual Exposure",
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
        
        h3("Upload Files"),
        tags$p(HTML("Upload the individual-level Residential History and Exposure Window files.")),
        
        div(class = "upload-block",
            div(class = "section-title", "Residential History (.csv/.xls/.xlsx)"),
            tags$small(class = "req",
                       HTML('Required columns: <code>IndividualID</code>, <code>PostalCode</code>, <code>StartDate</code>, <code>EndDate</code>')
            ),
            fileInput(
              "res_file", label = NULL,
              accept = c(".csv", ".xls", ".xlsx"),
              buttonLabel = "Select file",
              placeholder = "No file selected"
            ),
            # Inline error slot (appears directly under the chooser)
            uiOutput("res_size_error")
        ),
        
        div(class = "upload-block",
            div(class = "section-title", "Exposure Window (.csv/.xls/.xlsx)"),
            tags$small(class = "req",
                       HTML('Required columns: <code>IndividualID</code>, <code>ExposureStartDate</code>, <code>ExposureEndDate</code>')
            ),
            fileInput(
              "exp_file", label = NULL,
              accept = c(".csv", ".xls", ".xlsx"),
              buttonLabel = "Select file",
              placeholder = "No file selected"
            ),
            # Inline error slot (appears directly under the chooser)
            uiOutput("exp_size_error")
        ),
        
        # Client-side size gate: block >250MB selections and emit inline message
        tags$script(HTML("
(function(){
  var MAX = 250 * 1000 * 1000; // 250 MB

  function clearChooser(finput){
    if (!finput) return;
    finput.value = '';
    var wrap = finput.closest('.shiny-input-container');
    var mirror = wrap && wrap.querySelector('input[type=text]');
    if (mirror) mirror.value = '';
  }

  // Bind whether the id is on the wrapper OR the file input itself
  function bindOne(id, target){
    var finput =
      document.querySelector('#' + id + ' input[type=file]') ||   // file input inside wrapper
      document.querySelector('input[type=file]#' + id);           // file input IS the id

    if (!finput || finput.dataset.bound === '1') return;

    // Capture-phase listener so we intercept before Shiny's own handler
    finput.addEventListener('change', function(evt){
      var f = this.files && this.files[0];
      if (f && Number.isFinite(f.size) && f.size > MAX){
        evt.stopImmediatePropagation();
        evt.preventDefault();
        clearChooser(this);
        if (window.Shiny && Shiny.setInputValue){
          Shiny.setInputValue('client_upload_error',
            { target: target, name: f.name, size: f.size, ts: Date.now() },
            { priority: 'event' });
        }
        return false;
      }
    }, true);

    finput.dataset.bound = '1';
  }

  function attachAll(){ bindOne('res_file','res'); bindOne('exp_file','exp'); }

  // Attach robustly (initial render, reconnects, dynamic re-renders)
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', attachAll);
  } else {
    setTimeout(attachAll, 0);
  }
  document.addEventListener('shiny:connected', attachAll);
  document.addEventListener('shiny:value', attachAll);
  new MutationObserver(attachAll).observe(document.documentElement, {childList:true, subtree:true});
})();
")),
        # Client-side size gate for both fileInputs (redundant safety)
        tags$script(HTML("
(function(){
  var MAX = 250 * 1000 * 1000; // 250 MB

  function clearChooser(fileEl){
    if (!fileEl) return;
    // Clear the real file input
    fileEl.value = '';
    // Clear the mirrored text input Shiny renders
    var wrap = fileEl.closest('.shiny-input-container');
    var mirror = wrap && wrap.querySelector('input[type=text]');
    if (mirror) mirror.value = '';
  }

  function bindOne(wrapperId, targetKey){
    var wrap = document.getElementById(wrapperId);
    if (!wrap) return;
    var finput = wrap.querySelector('input[type=file]');
    if (!finput || finput.dataset.bound === '1') return;

    // Run before Shiny's listener
    finput.addEventListener('change', function(evt){
      var f = this.files && this.files[0];
      if (f && Number.isFinite(f.size) && f.size > MAX){
        evt.stopImmediatePropagation();
        evt.preventDefault();

        clearChooser(this);

        if (window.Shiny && Shiny.setInputValue){
          Shiny.setInputValue('client_upload_error',
            { target: targetKey, name: f.name, size: f.size, ts: Date.now() },
            { priority: 'event' });
        }
        return false;
      }
    }, true); // capture phase

    finput.dataset.bound = '1';
  }

  function attachAll(){
    bindOne('res_file', 'res');
    bindOne('exp_file', 'exp');
  }

  document.addEventListener('DOMContentLoaded', attachAll);
  document.addEventListener('shiny:connected', attachAll);
  document.addEventListener('shiny:value', attachAll);
  var mo = new MutationObserver(attachAll);
  mo.observe(document.documentElement, {childList:true,subtree:true});
})();
")),
        
        actionButton("clear_files", "Clear all", class = "btn btn-sm btn-outline-secondary"),
        
        div(style = "margin-top:16px;"),
        shinyjs::disabled(
          actionButton("run_analysis", "Calculate Metrics",
                       class = "btn btn-primary btn-lg", width = "100%")
        ),
        tags$small(textOutput("status_line"), style="display:block; margin-top:6px;")
      ),
      
      h3("Results", id = "results_hdr", class = "muted"),
      
      # Visible alert area for blocking errors (file issues etc.)
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
                             plotOutput("placeholder_plot", height = paste0(PROFILE_PLOT_HEIGHT_PX, "px")),
                             br(),
                             downloadButton("dl_selected_plot", "Download figure (selected ID)")
                    ),
                    tabPanel(
                      title = tagList(
                        "Warning/Error Log",
                        tags$span(
                          id = "warn_badge",
                          class = "badge rounded-pill ms-2",
                          ""
                        )
                      ),
                      br(),
                      DTOutput("tbl_log")
                    )
        )
      )
    )
  ),
  
  tabPanel(
    "About",
    fluidPage(
      fluidRow(
        column(
          width = 9,
          tags$h4(HTML("<b>About the Dashboard</b>")),
          HTML("
<p>This dashboard uses information on individual-level residential histories and defined exposure windows to generate weekly estimates of wildfire smoke (WFS) PM<sub>2.5</sub> and calculate the 12 Multiyear Wildfire Smoke Exposure (MultiWiSE) metrics. The MultiWiSE metrics characterize an individual's episodic exposure to WFS PM<sub>2.5</sub> over the duration of the provided multiyear exposure window. Using PM<sub>2.5</sub> estimates from CanOSSEM, the dashboard can be used to generate PM<sub>2.5</sub> exposure profiles and the MultiWiSE metrics for multiple individuals residing in Canada for any period between 2010–2023. An overview of the required input to the dashboard can be found below in 'Usage', and an overview of the provided outputs can be found in 'Outputs.' Additional details on the dashboard can be found in the README.</p>
"),
          
          tags$h4(HTML("<b>Usage</b>")),
          HTML("
<p>The dashboard relies on the user providing a residential history and exposure window for each individual. Two files in .csv, .xlsx, or .xls format, must be provided: 1) <strong>Residential History</strong> – For each individual, one row per postal code they lived in during the exposure window, with information on the dates they moved into and out of each postal code. Required columns include: <code>IndividualID</code>, <code>PostalCode</code>, <code>StartDate</code>, <code>EndDate</code>, and 2) <strong>Exposure Window</strong> – One row per individual with the start and end date of the exposure window of interest. This exposure window can be different for each individual. Required columns include: <code>IndividualID</code>, <code>ExposureStart</code>, and <code>ExposureEnd</code>. For the dashboard to work as expected, the uploaded files should meet the following conditions:</p>
"),
          tags$ul(
            tags$li("All dates must be written as YYYY-MM-DD (e.g., 2022-07-15)."),
            tags$li("The provided postal codes must be valid and located in one of the 13 provinces or territories in Canada. The postal codes must be formatted to have 6 digits and no spaces (e.g., V5Z4R4)"),
            tags$li("Each individual must appear in both the exposure window file and the residential history file"),
            tags$li("The residential history for an individual should be contiguous (i.e., no gaps) and non-overlapping, and the start date for a given postal code must occur before the end date"),
            tags$li(HTML("The exposure window for each individual must fall within the range of available PM<sub>2.5</sub> data (January 1, 2010 – December 31, 2023), be at least 1 year in length, and the exposure window start date must occur before the end date")),
            tags$li("The residential history for an individual must include residential information for the entirety of the exposure window"),
            tags$li("Each individual must appear exactly once in the exposure window file. Multiple exposure windows for the same individual will not be processed"),
            tags$li("No missing values in either file")
          ),
          HTML("
<p>If any of the above conditions are not met, the dashboard will either: (1) automatically handle the issue and store an associated warning message detailing how the issue was addressed, or (2) stop running entirely or skip the problematic individual(s) and store an error message detailing what the issue was.</p>"),
          tags$h5(HTML("<i>Running the dashboard using larger files</i>")),
          HTML("
<p>If you would like to process files that are greater than 250 MB in size, you can run the dashboard locally on your machine. To do so, download the code and files provided here: <a href='https://github.com/sfu-fhs-cleland/MultiWiSE_Canada_Dashboard' target='_blank'>https://github.com/sfu-fhs-cleland/MultiWiSE_Canada_Dashboard</a>. You can then open app.R in RStudio, hit 'Run App,' and proceed with processing your larger files.</p>"),
          
          tags$h4(HTML("<b>Outputs</b>")),
          HTML("
<p>There are four individual-level outputs provided by the dashboard. Each output is calculated over the duration of the provided exposure window:</p>
"),
          tags$ol(
            tags$li(HTML("<strong>12 MultiWiSE metrics</strong> and five additional variables.")),
            tags$li(HTML(paste0(
              "Three figures of <strong>PM<sub>2.5</sub> exposure profiles</strong>:",
              tags$ul(
                tags$li(HTML("Cumulative PM<sub>2.5</sub> exposure, separated into total, WFS, and non-WFS components.")),
                tags$li(HTML("Weekly average total, WFS, and non-WFS PM<sub>2.5</sub> concentrations, with identification of WFS episodes and severe WFS episodes.")),
                tags$li(HTML("Distribution of the weekly sum of total PM<sub>2.5</sub> exposure, with indication of the range and median used to identify the counterfactual weekly value."))
              )
            ))),
            tags$li(HTML("<strong>Weekly PM<sub>2.5</strong> estimates</strong> (total, WFS, and non-WFS PM<sub>2.5</sub> and WFS episodes).")),
            tags$li(HTML("<strong>Log file</strong> with warning and error messages."))
          ),
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
              tags$p("Download the README for more information:"),
              tags$a(
                id    = "dl_all_zip_about",
                class    = "btn btn-primary btn-lg w-100",
                href  = "README_102225.pdf",
                download = "MultiWiSE_Canada_README.pdf",
                target = "_blank",
                HTML("<b>Download README (PDF)</b>")
              )
            )
          )
        )
      )
    )
  )
)


#### SERVER ####
server <- function(input, output, session) {
  # ---------- state ----------
  v <- reactiveValues(
    ids = character(0),
    data_by_id = list(),
    metrics_df = NULL,
    weekly_df  = NULL,
    log_df     = NULL,
    file_ok    = FALSE,
    last_run_dir = NULL,
    res_valid = FALSE,
    exp_valid = FALSE
  )
  
  # Centralized oversize message + inline helper
  # NOTE: No maximum upload size for dashboard to run locally
  # OVERSIZE_MSG <- "Maximum upload size is 250 MB. Please see 'About' for how to run the dashboard using larger files."
  # make_inline_error <- function(msg, title = "Error") {
  #   div(class = "inline-error upload-msg",
  #       if (!is.null(title) && nzchar(title)) span(class = "err-title", paste0(title, ":")),
  #       span(as.character(msg)))
  # }
  
  # Prep indicator is intentionally disabled (no-op handlers)
  show_prep <- function(...) {}
  hide_prep <- function(...) {}
  
  # ---- alert helper ----
  bslib_has_alert <- function() {
    "bslib" %in% .packages(all.available = TRUE) &&
      "alert" %in% getNamespaceExports("bslib")
  }
  make_alert <- function(msg, title = "Error") {
    if (bslib_has_alert()) {
      bslib::alert(title = title, msg, severity = "danger")
    } else {
      div(class = "alert alert-danger", role = "alert",
          tags$strong(paste0(title, ": ")),
          tags$span(as.character(msg)))
    }
  }
  update_warn_badge <- function(issues) {
    if (isTRUE(issues > 0)) {
      shinyjs::html("warn_badge", as.character(issues))
      shinyjs::removeClass("warn_badge", "bg-secondary")
      shinyjs::addClass("warn_badge", "bg-warning text-white d-inline-flex")
      shinyjs::show("warn_badge")
    } else {
      shinyjs::html("warn_badge", "")
      shinyjs::hide("warn_badge")
      shinyjs::removeClass("warn_badge", "bg-warning text-white d-inline-flex")
      shinyjs::addClass("warn_badge", "bg-secondary")
    }
  }
  update_ready_state <- function() {
    v$file_ok <- isTRUE(v$res_valid) && isTRUE(v$exp_valid)
    if (v$file_ok) {
      shinyjs::enable("run_analysis")
      output$status_line <- renderText("Files look good. Ready to calculate.")
    } else {
      shinyjs::disable("run_analysis")
      if (!v$res_valid || !v$exp_valid) output$status_line <- renderText("")
    }
  }
  
  # ---- initial UI state ----
  shinyjs::disable("run_analysis")
  shinyjs::disable("dl_all_zip")
  shinyjs::addClass("download_bar", "muted")
  output$run_alert <- renderUI(NULL)
  output$status_line <- renderText("")
  shinyjs::addClass("results_wrap", "muted")
  shinyjs::addClass("results_hdr",  "muted")
  output$res_size_error <- renderUI(NULL)
  output$exp_size_error <- renderUI(NULL)
  
  output$tbl_metrics <- DT::renderDT({
    DT::datatable(tibble::tibble(), rownames = FALSE, options = list(pageLength = 10))
  })
  output$tbl_weekly <- DT::renderDT({
    DT::datatable(tibble::tibble(), rownames = FALSE, options = list(pageLength = 10))
  })
  output$tbl_log <- DT::renderDT({
    DT::datatable(tibble::tibble(IndividualID=character(), Message=character()),
                  rownames = FALSE, options = list(pageLength = 10))
  })
  output$id_selector_ui <- renderUI({
    div(class = "text-muted", HTML("Select an ID will appear here after you calculate metrics."))
  })
  output$placeholder_plot <- renderPlot({
    par(mar = c(0,0,0,0)); plot.new()
    text(0.5, 0.55, "No figures yet", cex = 1.5)
    text(0.5, 0.45, "Upload files and click \"Calculate Metrics\".")
  })
  
  # ---------- Clear ----------
  observeEvent(input$clear_files, ignoreInit = TRUE, {
    shinyjs::reset("res_file")
    shinyjs::reset("exp_file")    
    v$ids <- character(0)
    v$file_ok <- FALSE
    v$last_run_dir <- NULL
    v$res_valid <- FALSE
    v$exp_valid <- FALSE
    shinyjs::disable("run_analysis")
    shinyjs::disable("dl_all_zip")
    shinyjs::addClass("download_bar", "muted")
    output$run_alert <- renderUI(NULL)
    output$status_line <- renderText("")
    shinyjs::addClass("results_wrap", "muted")
    shinyjs::addClass("results_hdr",  "muted")
    output$res_size_error <- renderUI(NULL)
    output$exp_size_error <- renderUI(NULL)
    update_warn_badge(0)
    hide_prep()
    
    output$tbl_metrics <- DT::renderDT({
      DT::datatable(tibble::tibble(), rownames = FALSE, options = list(pageLength = 10))
    })
    output$tbl_weekly <- DT::renderDT({
      DT::datatable(tibble::tibble(), rownames = FALSE, options = list(pageLength = 10))
    })
    output$tbl_log <- DT::renderDT({
      DT::datatable(tibble::tibble(IndividualID=character(), Message=character()),
                    rownames = FALSE, options = list(pageLength = 10))
    })
    output$id_selector_ui <- renderUI({
      div(class = "text-muted", HTML("Select an ID will appear here after you calculate metrics."))
    })
    output$placeholder_plot <- renderPlot({
      par(mar = c(0,0,0,0)); plot.new()
      text(0.5, 0.55, "No figures yet", cex = 1.5)
      text(0.5, 0.45, "Upload files and click \"Calculate Metrics\".")
    })
  })
  
  # ---------- Handle client-side oversize events ----------
  observeEvent(input$client_upload_error, ignoreInit = TRUE, {
    tgt <- tryCatch(input$client_upload_error$target, error = function(e) NULL)
    if (identical(tgt, "res")) {
      shinyjs::reset("res_file")
      v$res_valid <- FALSE
      # output$res_size_error <- renderUI(make_inline_error(OVERSIZE_MSG)) # NOTE: No cap on upload size for dashboard to run locally
    } else if (identical(tgt, "exp")) {
      shinyjs::reset("exp_file")
      v$exp_valid <- FALSE
      # output$exp_size_error <- renderUI(make_inline_error(OVERSIZE_MSG)) # NOTE: No cap on upload size for dashboard to run locally
    }
    output$run_alert <- renderUI(NULL)
    output$status_line <- renderText("")
    update_ready_state()
    hide_prep()
  })
  
  # ---------- Validate file inputs ----------
  observeEvent(input$res_file, ignoreInit = TRUE, {
    output$run_alert <- renderUI(NULL)
    output$res_size_error <- renderUI(NULL)  # Clear inline message on valid pick
    v$res_valid <- FALSE
    update_ready_state()
    
    if (is.null(input$res_file)) return()
    
    # Server-side size gate (matches 250 MB cap)
    # NOTE: No cap on upload size for dashboard to run locally
    # reported_size <- if (!is.null(input$res_file$size) && length(input$res_file$size)) input$res_file$size[1] else NA_real_
    # disk_size <- tryCatch({
    #   p <- input$res_file$datapath
    #   if (!is.null(p) && nzchar(p) && file.exists(p)) file.info(p)$size else NA_real_
    # }, error = function(e) NA_real_)
    # actual_size <- suppressWarnings(if (is.finite(reported_size)) reported_size else disk_size)
    # if (is.finite(actual_size) && actual_size > MAX_UPLOAD_BYTES) {
    #   shinyjs::reset("res_file")
    #   v$res_valid <- FALSE
    #   output$res_size_error <- renderUI(make_inline_error(OVERSIZE_MSG))
    #   output$status_line <- renderText("")
    #   hide_prep()
    #   update_ready_state()
    #   return(invisible())
    # }
    
    show_prep("Reading & validating Residential History…")
    
    invisible(tryCatch({
      res_df <- read_and_check(
        path          = input$res_file$datapath,
        required_cols = required_cols_res,
        file_label    = "Residential History"
      )
      v$res_valid <- TRUE
      
      if (v$exp_valid) {
        hide_prep()
      } else {
        show_prep("Residential History OK. Waiting for Exposure Window…")
      }
      
      if (v$exp_valid) v$ids <- sort(unique(as.character(res_df$IndividualID)))
      update_ready_state()
    }, error = function(e) {
      output$run_alert <- renderUI(make_alert(conditionMessage(e)))
      v$res_valid <- FALSE
      update_ready_state()
      show_prep("Issue detected. See message above.")
      NULL
    }))
  })
  
  observeEvent(input$exp_file, ignoreInit = TRUE, {
    output$run_alert <- renderUI(NULL)
    output$exp_size_error <- renderUI(NULL)  # Clear inline message on valid pick
    v$exp_valid <- FALSE
    update_ready_state()
    if (is.null(input$exp_file)) return()
    
    # Server-side size gate (matches 250 MB cap)
    # NOTE: No cap on upload size for dashboard to run locally
    # reported_size <- if (!is.null(input$exp_file$size) && length(input$exp_file$size)) input$exp_file$size[1] else NA_real_
    # disk_size <- tryCatch({
    #   p <- input$exp_file$datapath
    #   if (!is.null(p) && nzchar(p) && file.exists(p)) file.info(p)$size else NA_real_
    # }, error = function(e) NA_real_)
    # actual_size <- suppressWarnings(if (is.finite(reported_size)) reported_size else disk_size)
    # if (is.finite(actual_size) && actual_size > MAX_UPLOAD_BYTES) {
    #   shinyjs::reset("exp_file")
    #   v$exp_valid <- FALSE
    #   output$exp_size_error <- renderUI(make_inline_error(OVERSIZE_MSG))
    #   output$status_line <- renderText("")
    #   hide_prep()
    #   update_ready_state()
    #   return(invisible())
    # }
    
    show_prep("Reading & validating Exposure Window…")
    
    invisible(tryCatch({
      exp_df <- read_and_check(
        path          = input$exp_file$datapath,
        required_cols = required_cols_exp,
        file_label    = "Exposure Window"
      )
      v$exp_valid <- TRUE
      
      if (v$res_valid) {
        hide_prep()
      } else {
        show_prep("Exposure Window OK. Waiting for Residential History…")
      }
      
      update_ready_state()
    }, error = function(e) {
      output$run_alert <- renderUI(make_alert(conditionMessage(e)))
      v$exp_valid <- FALSE
      update_ready_state()
      show_prep("Issue detected. See message above.")
      NULL
    }))
  })
  
  # ---------- Calculate Metrics ----------
  observeEvent(input$run_analysis, {
    tryCatch({
      req(v$file_ok, input$res_file, input$exp_file)
      output$run_alert <- renderUI(NULL)
      hide_prep()
      
      # Server-side size gate at run time (matches 250 MB cap)
      # NOTE: No cap on upload size for dashboard to run locally
      # s_res <- tryCatch({
      #   p <- input$res_file$datapath
      #   if (!is.null(p) && nzchar(p) && file.exists(p)) file.info(p)$size else NA_real_
      # }, error = function(e) NA_real_)
      # s_exp <- tryCatch({
      #   p <- input$exp_file$datapath
      #   if (!is.null(p) && nzchar(p) && file.exists(p)) file.info(p)$size else NA_real_
      # }, error = function(e) NA_real_)
      # if (is.finite(s_res) && s_res > MAX_UPLOAD_BYTES) {
      #   output$res_size_error <- renderUI(make_inline_error(OVERSIZE_MSG))
      #   output$status_line <- renderText("")
      #   shinyjs::disable("run_analysis")
      #   return(invisible())
      # }
      # if (is.finite(s_exp) && s_exp > MAX_UPLOAD_BYTES) {
      #   output$exp_size_error <- renderUI(make_inline_error(OVERSIZE_MSG))
      #   output$status_line <- renderText("")
      #   shinyjs::disable("run_analysis")
      #   return(invisible())
      # }
      
      token_path_now <- resolve_token_path()
      if (is.na(token_path_now) || !file.exists(token_path_now)) {
        output$run_alert <- renderUI(make_alert(
          "We ran into issues when trying to access the data on Dropbox. Please contact us to let us know this issue occurred and we will do our best to address the issue as soon as possible."
        ))
        return()
      }
      
      shinyjs::disable("dl_all_zip")
      shinyjs::addClass("download_bar", "muted")
      shinyjs::addClass("results_wrap", "muted")
      shinyjs::addClass("results_hdr",  "muted")
      update_warn_badge(0)
      output$status_line <- renderText("Fetching data from Dropbox...")
      
      run_dir <- file.path(out_dir_default, paste0("run_", format(Sys.time(), "%Y%m%d_%H%M%S")))
      dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
      
      withProgress(message = "Fetching data from Dropbox...", value = 0, {
        res_df <- read_and_check(
          path          = input$res_file$datapath,
          required_cols = required_cols_res,
          file_label    = "Residential History"
        )
        exp_df <- read_and_check(
          path          = input$exp_file$datapath,
          required_cols = required_cols_exp,
          file_label    = "Exposure Window"
        )
        
        res_df <- res_df %>% mutate(StartDate = as.Date(StartDate), EndDate = as.Date(EndDate))
        exp_df <- exp_df %>% mutate(ExposureStartDate = as.Date(ExposureStartDate),
                                    ExposureEndDate   = as.Date(ExposureEndDate))
        
        res_ids <- unique(res_df$IndividualID[!is.na(res_df$PostalCode)])
        exp_ids <- unique(exp_df$IndividualID[!is.na(exp_df$ExposureStartDate)])
        
        missing_in_exp <- setdiff(res_ids, exp_ids)
        if (length(missing_in_exp)) {
          stop(sprintf(
            "Unable to process data. There is a mismatch between the IndividualIDs in the residential history and exposure window files. The following IDs are included in the residential history file but not in the exposure window file: %s. Please ensure that the files have the same number of IndividualIDs, and that each IndividualID exists in both files.",
            paste(missing_in_exp, collapse = ", ")
          ), call. = FALSE)
        }
        
        missing_in_res <- setdiff(exp_ids, res_ids)
        if (length(missing_in_res)) {
          stop(sprintf(
            "Unable to process data. There is a mismatch between the IndividualIDs in the exposure window file and the residential history file. The following IDs are included in the exposure window file but not in the residential history file: %s. Please ensure that the files have the same number of IndividualIDs, and that each IndividualID exists in both files.",
            paste(missing_in_res, collapse = ", ")
          ), call. = FALSE)
        }
        
        histories <- merge(res_df, exp_df, by = "IndividualID", all = TRUE, sort = FALSE)
        
        ids_all <- unique(histories$IndividualID[!is.na(histories$IndividualID)])
        ids_all <- ids_all[order(ids_all)]
        n <- length(ids_all); if (n == 0) stop("No IndividualID values detected after merging files.", call. = FALSE)
        
        setProgress(value = 0, message = "Fetching data from Dropbox...", detail = "Initializing…")
        
        warnings_collected <- list()
        errors_collected   <- list()
        out_list <- list()
        
        i <- 0L
        dt <- data.table::as.data.table(histories)
        data.table::setorder(dt, IndividualID, StartDate)
        by_id <- split(dt, dt$IndividualID)
        
        for (id in ids_all) {
          i <- i + 1L
          setProgress(value = (i-1)/max(1,n), message = "Processing individual data",
                      detail = sprintf("Processing individual %d of %d…", i, n))
          
          d <- by_id[[as.character(id)]]
          warns <- character(); errs <- character()
          res <- tryCatch(
            withCallingHandlers(
              fn_single(d, token_path = token_path_now),
              warning = function(w) {
                m <- conditionMessage(w)
                if (!grepl('warnings? in .*summarise\\(', m, ignore.case = TRUE)) warns <<- c(warns, m)
                invokeRestart("muffleWarning")
              }
            ),
            error = function(e) { errs <<- c(errs, conditionMessage(e)); NULL }
          )
          msg <- if (length(errs)) paste(unique(errs), collapse = "; ")
          else if (length(warns)) paste(unique(warns), collapse = "; ")
          else "No issues"
          
          if (!exists("log_dt_local")) log_dt_local <- data.table::data.table(IndividualID=character(), Message=character())
          log_dt_local <- rbind(log_dt_local, data.table::data.table(IndividualID=id, Message=msg))
          if (!is.null(res)) out_list[[as.character(id)]] <- res
          
          incProgress(1/max(1,n))
        }
        
        setProgress(value = 1, message = "Generating plots and tables", detail = "Preparing outputs…")
        output$status_line <- renderText("Generating plots and tables...")
        
        if (length(out_list) > 0) {
          metrics_wide <- data.table::rbindlist(
            lapply(names(out_list), function(id) {
              df <- out_list[[id]]$metrics
              df$IndividualID <- id
              data.table::as.data.table(df)[, c("IndividualID",
                                                setdiff(names(df), "IndividualID")), with = FALSE]
            }), use.names = TRUE, fill = TRUE
          )
          plots_list <- list()  # Figures are built on demand
          data_list  <- setNames(lapply(out_list, `[[`, "data"), names(out_list))
          
          batch <- list(metrics_wide = metrics_wide,
                        plots        = plots_list,
                        data         = data_list)
          
          save_batch_outputs(batch, out_dir_root = run_dir)
          
          if (exists("log_dt_local")) {
            data.table::fwrite(log_dt_local, file = file.path(run_dir, "Warning_Error_Log.csv"))
          }
          
          long_weekly <- data.table::rbindlist(data_list, idcol = "IndividualID", use.names = TRUE)
          long_weekly[, epiweek_index_rel := seq_len(.N), by = IndividualID]
          cols_keep <- c(
            "IndividualID", "PostalCode", "epiweek_index_rel", "n_days",
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
          
          v$metrics_df <- as.data.frame(metrics_wide)
          v$weekly_df  <- as.data.frame(long_weekly)
          v$data_by_id <- data_list
          v$ids        <- names(data_list)
          v$log_df     <- if (exists("log_dt_local")) as.data.frame(log_dt_local) else data.frame(IndividualID=character(), Message=character())
          
          # Rounding for Weekly tab display (parity with CSV)
          if (!is.null(v$weekly_df)) {
            avg_cols <- c("weekly_pm25_avg", "weekly_wfs_pm25_avg", "weekly_non_wfs_pm25_avg")
            sum_cols <- c("weekly_pm25_sum", "weekly_wfs_pm25_sum", "weekly_non_wfs_pm25_sum")
            
            for (cl in intersect(avg_cols, names(v$weekly_df))) {
              v$weekly_df[[cl]] <- signif(as.numeric(v$weekly_df[[cl]]), 3)
            }
            for (cl in intersect(sum_cols, names(v$weekly_df))) {
              v$weekly_df[[cl]] <- signif(as.numeric(v$weekly_df[[cl]]), 4)
            }
          }
          
        } else {
          v$metrics_df <- NULL
          v$weekly_df  <- NULL
          v$data_by_id <- list()
          v$ids        <- character(0)
          v$log_df     <- if (exists("log_dt_local")) as.data.frame(log_dt_local) else data.frame(IndividualID=character(), Message=character())
        }
        
        v$last_run_dir <- run_dir
      }) # withProgress
      
      # ---- render to UI ----
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
        selectInput("sel_id", "Individual ID", choices = v$ids, selected = v$ids[[1]], width = "320px")
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
  
  # ---------- Download selected figure (build on demand) ----------
  output$dl_selected_plot <- downloadHandler(
    filename = function() {
      id <- input$sel_id %||% "selected"
      ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
      sprintf("MultiWiSE_Figure_%s_%s.png", id, ts)
    },
    content = function(file) {
      req(input$sel_id, v$data_by_id[[input$sel_id]])
      g <- {
        df <- v$data_by_id[[input$sel_id]]
        p1 <- fn_plot_cumulative_sum_by_year(df)
        p2 <- fn_plot_time_series_weekly_mean(df)
        p3 <- fn_plot_histogram(df)
        (p1 / p2 / p3) +
          patchwork::plot_annotation(
            title = input$sel_id,
            theme = ggplot2::theme(plot.title = ggplot2::element_text(size = 20, face = "bold", hjust = 0.5))
          )
      }
      ggplot2::ggsave(filename = file, plot = g, width = 14, height = 28, dpi = 300)
    }
  )
  
  # ---------- ZIP download (bundle the latest run_dir) ----------
  do_zip <- function(path_out) {
    staging <- file.path(tempdir(), paste0("mw_bundle_", as.integer(Sys.time()), "_", Sys.getpid()))
    on.exit(try(unlink(staging, recursive = TRUE, force = TRUE), silent = TRUE), add = TRUE)
    dir.create(staging, recursive = TRUE, showWarnings = FALSE)
    
    if (!is.null(v$last_run_dir) && dir.exists(v$last_run_dir)) {
      run_files <- list.files(v$last_run_dir, full.names = TRUE, all.files = FALSE, no.. = TRUE)
      if (length(run_files)) file.copy(run_files, to = staging, recursive = TRUE, overwrite = TRUE)
    }
    zip_dir(staging, zip_path = path_out)
  }
  output$dl_all_zip <- downloadHandler(
    filename = function() {
      ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
      sprintf("MultiWiSE_Individual_Results_%s.zip", ts)
    },
    content = function(file) do_zip(file)
  )
  observeEvent(input$zip_clicked, ignoreInit = TRUE, {
    showNotification("Preparing ZIP...", type = "message", duration = 3)
  })
}


#### Run the app ####
shinyApp(ui = ui, server = server)
