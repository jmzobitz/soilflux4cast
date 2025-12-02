#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) stop("Usage: Rscript scripts/combine_month.R YYYY-MM")
curr_month <- as.character(args[1])

suppressPackageStartupMessages({
  library(tidyverse)
  library(stringr)
})

message("[combine_month] month=", curr_month)

# Find env files recursively (works whether artifacts were copied or left under site-*/)
env_files <- list.files(path = ".", pattern = paste0("soil_drivers-.*-", curr_month, "\\.csv$"), recursive = TRUE, full.names = TRUE) |> stringr::str_subset("soil_drivers-")

if (length(env_files) > 0) {
  out_env <- tibble(
    site_id = str_extract(basename(env_files), "(?<=soil_drivers-)[A-Za-z0-9]{4}"),
    values = lapply(env_files, readr::read_csv)
  ) |> unnest(values)

  dir.create(file.path("data"), showWarnings = FALSE, recursive = TRUE)
  out_env_file <- file.path("data", "drivers", paste0("neon_soil_drivers-", curr_month, ".csv"))
  readr::write_csv(out_env, out_env_file)
  message("[combine_month] wrote env combined file: ", out_env_file)
} else {
  message("[combine_month] no env files found (looked for: soil_drivers-*-", curr_month, ".csv)")
}

# Flux files (search recursively)
flux_files <- list.files(path = ".", pattern = paste0("forecast_prediction-.*-", curr_month, "\\.csv$"), recursive = TRUE, full.names = TRUE) |> stringr::str_subset("forecast_prediction-")

if (length(flux_files) > 0) {
  out_flux <- tibble(
    site_id = str_extract(basename(flux_files), "(?<=forecast_prediction-)[A-Za-z0-9]{4}"),
    values = lapply(flux_files, readr::read_csv)
  ) |>
    select(-site_id) |>
    unnest(values) |>
    ungroup()

  dir.create(file.path("data", "targets"), showWarnings = FALSE, recursive = TRUE)
  out_flux_file <- file.path("data", "targets", paste0("neon_targets-", curr_month, ".csv"))
  readr::write_csv(out_flux, out_flux_file)
  message("[combine_month] wrote flux combined file: ", out_flux_file)
} else {
  message("[combine_month] no flux files found (looked for: forecast_prediction-*-", curr_month, ".csv)")
}

message("[combine_month] finished")
