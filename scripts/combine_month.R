#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) stop("Usage: Rscript scripts/combine_month.R YYYY-MM")
curr_month <- as.character(args[1])

suppressPackageStartupMessages({
  library(tidyverse)
  library(stringr)
})

message("[combine_month] month=", curr_month)

# Env files
env_files <- list.files(path = "data/drivers/neon",
                        full.names = TRUE
) |>
  stringr::str_subset(pattern = paste0("(?<=[A-Za-z0-9]{4}-)", curr_month, "\\.csv$"))

if (length(env_files) > 0) {
  out_env <- tibble(
    site_id = str_extract(env_files, "(?<=soil_drivers-)[A-Za-z0-9]{4}"),
    values = lapply(env_files, readr::read_csv)
  ) |> unnest(values)
  
  out_env_file <- file.path("data", "drivers", paste0("neon_soil_drivers-", curr_month, ".csv"))
  readr::write_csv(out_env, out_env_file)
  file.remove(env_files)
  message("[combine_month] wrote env combined file: ", out_env_file)
} else {
  message("[combine_month] no env files found")
}

# Flux files
flux_files <- list.files(path = "data/targets/neon", 
                         full.names = TRUE
) |>
  stringr::str_subset(pattern = paste0("(?<=[A-Za-z0-9]{4}-)", curr_month, "\\.csv$"))


if (length(flux_files) > 0) {
  out_flux <- tibble(
    site_id = str_extract(flux_files, "(?<=forecast_prediction-)[A-Za-z0-9]{4}"),
    values = lapply(flux_files, readr::read_csv)
  ) |>
    select(-site_id) |>
    unnest(values) |>
    ungroup()
  
  out_flux_file <- file.path("data", "targets", paste0("neon_targets-", curr_month, ".csv"))
  readr::write_csv(out_flux, out_flux_file)
  file.remove(flux_files)
  message("[combine_month] wrote flux combined file: ", out_flux_file)
} else {
  message("[combine_month] no flux files found")
}

message("[combine_month] finished")
