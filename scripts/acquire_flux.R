#!/usr/bin/env Rscript

# Compute flux for all terrestrial NEON sites for a given month (YYYY-MM)
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) stop("Usage: Rscript scripts/acquire_flux.R YYYY-MM")
curr_month <- as.character(args[1])

suppressPackageStartupMessages({
  library(neonSoilFlux)
  library(tidyverse)
  library(neonUtilities)
  library(lubridate)
})

message("[acquire_flux] month=", curr_month)

out_dir <- file.path("data", "targets", "neon")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

site_meta_url <- "https://raw.githubusercontent.com/eco4cast/neon4cast-targets/main/NEON_Field_Site_Metadata_20220412.csv"
site_names <- tryCatch(
  readr::read_csv(site_meta_url, show_col_types = FALSE) |>
    filter(terrestrial == 1) |>
    pull(field_site_id),
  error = function(e) stop("Failed to download NEON site metadata: ", e$message)
)

# conversion factor
conv <- 1e-6 * 0.012011 * 1000 * 1800

for (curr_site in site_names) {
  message("[acquire_flux] site=", curr_site)
  try({
    out_env_data <- acquire_neon_data(
      site_name = curr_site,
      download_date = curr_month,
      provisional = TRUE
    )
    
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
      mutate(site_id = curr_site) |>
      relocate(site_id) |>
      arrange(startDateTime)
    
    out_file <- file.path(out_dir, paste0("forecast_prediction-", curr_site, "-", curr_month, ".csv"))
    readr::write_csv(flux_full, out_file)
    message("[acquire_flux] wrote: ", out_file)
  }, silent = FALSE)
}

message("[acquire_flux] done")