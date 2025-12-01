#!/usr/bin/env Rscript

# Process a single site (env + flux). Usage: Rscript scripts/process_single_site.R SITE_ID YYYY-MM
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) stop("Usage: Rscript scripts/process_single_site.R SITE_ID YYYY-MM")
site_id <- as.character(args[1])
curr_month <- as.character(args[2])

suppressPackageStartupMessages({
  library(neonSoilFlux)
  library(tidyverse)
  library(neonUtilities)
  library(lubridate)
})

message("[process_single_site] site=", site_id, " month=", curr_month)

# output dirs
env_dir <- file.path("data", "drivers", "neon")
flux_dir <- file.path("data", "targets", "neon")
if (!dir.exists(env_dir)) dir.create(env_dir, recursive = TRUE)
if (!dir.exists(flux_dir)) dir.create(flux_dir, recursive = TRUE)

# conversion factor
conv <- 1e-6 * 0.012011 * 1000 * 1800

try({
  out_env_data <- acquire_neon_data(
    site_name = site_id,
    download_date = curr_month,
    provisional = TRUE
  )
  
  # VSWC
  VSWC_data <- out_env_data$site_data$data[[2]] |>
    filter(
      verticalPosition == "501",
      VSWCFinalQF == 0,
      hour(startDateTime) == 0,
      minute(startDateTime) == 0
    ) |>
    group_by(startDateTime) |>
    summarize(VSWC = mean(VSWCMean, na.rm = TRUE), .groups = "drop")
  
  # SOILT
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
  env_out_file <- file.path(env_dir, paste0("soil_drivers-", site_id, "-", curr_month, ".csv"))
  readr::write_csv(env_joined, env_out_file)
  message("[process_single_site] wrote env: ", env_out_file)
  
  # flux
  flux_full <- compute_neon_flux(
    input_site_env = out_env_data$site_data,
    input_site_megapit = out_env_data$site_megapit
  ) |>
    select(-any_of("surface_diffusivity")) |>
    unnest(cols = c(flux_compute)) |>
    group_by(startDateTime) |>
    mutate(
      flux = flux * conv,
      flux_err = flux_err * conv
    ) |>
    summarize(
      flux_out = mean(flux, na.rm = TRUE),
      flux_err = sd(flux, na.rm = TRUE) / sqrt(sum(!is.na(flux))),
      .groups = "drop"
    ) |>
    rename(flux = flux_out) |>
    group_by(startDateTime = floor_date(startDateTime, unit = "day")) |>
    summarize(
      flux = sum(flux, na.rm = TRUE),
      flux_err = sqrt(sum(flux_err^2, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    ungroup() |>
    mutate(site_id = site_id) |>
    relocate(site_id) |>
    arrange(startDateTime)
  
  flux_out_file <- file.path(flux_dir, paste0("forecast_prediction-", site_id, "-", curr_month, ".csv"))
  readr::write_csv(flux_full, flux_out_file)
  message("[process_single_site] wrote flux: ", flux_out_file)
}, silent = FALSE)

message("[process_single_site] finished")