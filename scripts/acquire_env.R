#!/usr/bin/env Rscript

# Acquire env data for all terrestrial NEON sites for a given month (YYYY-MM)
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) stop("Usage: Rscript scripts/acquire_env.R YYYY-MM")
curr_month <- as.character(args[1])

suppressPackageStartupMessages({
  library(neonSoilFlux)
  library(tidyverse)
  library(neonUtilities)
  library(lubridate)
})

message("[acquire_env] month=", curr_month)

# ensure output dir exists
out_dir <- file.path("data", "drivers", "neon")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# read site list
site_meta_url <- "https://raw.githubusercontent.com/eco4cast/neon4cast-targets/main/NEON_Field_Site_Metadata_20220412.csv"
site_names <- tryCatch(
  readr::read_csv(site_meta_url, show_col_types = FALSE) |>
    filter(terrestrial == 1) |>
    pull(field_site_id),
  error = function(e) {
    stop("Failed to download NEON site metadata: ", e$message)
  }
)

for (curr_site in site_names) {
  message("[acquire_env] site=", curr_site)
  try({
    out_env_data <- acquire_neon_data(
      site_name = curr_site,
      download_date = curr_month,
      provisional = TRUE
    )
    
    # VSWC
    VSWC_data <- try(
      out_env_data$site_data$data[[2]] |>
        filter(
          verticalPosition == "501",
          VSWCFinalQF == 0,
          hour(startDateTime) == 0,
          minute(startDateTime) == 0
        ) |>
        group_by(startDateTime) |>
        summarize(VSWC = mean(VSWCMean, na.rm = TRUE), .groups = "drop"),
      silent = TRUE
    )
    
    # SOILT
    SOILT_data <- try(
      out_env_data$site_data$data[[3]] |>
        filter(
          verticalPosition == "501",
          soilTempFinalQF == 0,
          hour(startDateTime) == 0,
          minute(startDateTime) == 0
        ) |>
        group_by(startDateTime) |>
        summarize(SOILT = mean(soilTempMean, na.rm = TRUE), .groups = "drop"),
      silent = TRUE
    )
    
    env_joined <- dplyr::full_join(
      if (inherits(SOILT_data, "try-error")) tibble(startDateTime = as.POSIXct(character()), SOILT = numeric()) else SOILT_data,
      if (inherits(VSWC_data, "try-error")) tibble(startDateTime = as.POSIXct(character()), VSWC = numeric()) else VSWC_data,
      by = "startDateTime"
    )
    
    out_file <- file.path(out_dir, paste0("soil_drivers-", curr_site, "-", curr_month, ".csv"))
    readr::write_csv(env_joined, out_file)
    message("[acquire_env] wrote: ", out_file)
  }, silent = FALSE)
}

message("[acquire_env] done")