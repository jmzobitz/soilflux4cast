#!/usr/bin/env Rscript

# Acquire site-level NEON env data for a given month
args <- commandArgs(trailingOnly = TRUE)
curr_month <- as.character(args[1])

library(neonSoilFlux)
library(tidyverse)
library(neonUtilities)
library(lubridate)

message("Running acquire_env.R for month: ", curr_month)

## --- Load NEON site list ------------------------------------------------------

site_names <- readr::read_csv(
  "https://raw.githubusercontent.com/eco4cast/neon4cast-targets/main/NEON_Field_Site_Metadata_20220412.csv",
  show_col_types = FALSE
) |>
  filter(terrestrial == 1) |>
  select(site_id = field_site_id)

## --- Loop over all sites ------------------------------------------------------

for (curr_site_name in site_names$site_id) {
  message("Processing site: ", curr_site_name)
  
  out_env_data <- try(
    acquire_neon_data(
      site_name = curr_site_name,
      download_date = curr_month,
      provisional = TRUE
    ),
    silent = TRUE
  )
  
  if (inherits(out_env_data, "try-error")) {
    message("⚠️ NEON acquisition failed for site: ", curr_site_name)
    next
  }
  
  # Extract VSWC
  VSWC_data <- out_env_data$site_data$data[[2]] |>
    filter(
      verticalPosition == "501",
      VSWCFinalQF == 0,
      hour(startDateTime) == 0,
      minute(startDateTime) == 0
    ) |>
    group_by(startDateTime) |>
    summarize(VSWC = mean(VSWCMean, na.rm = TRUE), .groups = "drop")
  
  # Extract SOILT
  SOILT_data <- out_env_data$site_data$data[[3]] |>
    filter(
      verticalPosition == "501",
      soilTempFinalQF == 0,
      hour(startDateTime) == 0,
      minute(startDateTime) == 0
    ) |>
    group_by(startDateTime) |>
    summarize(SOILT = mean(soilTempMean, na.rm = TRUE), .groups = "drop")
  
  env_joined <- full_join(SOILT_data, VSWC_data, by = "startDateTime")
  
  # Output filename
  out_file <- file.path(
    "data/drivers/neon",
    paste0("soil_drivers-", curr_site_name, "-", curr_month, ".csv")
  )
  
  readr::write_csv(env_joined, out_file)
  message("✓ Wrote env file: ", out_file)
}

message("Finished acquire_env.R")
