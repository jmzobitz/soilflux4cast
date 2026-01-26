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
  library(zoo)
})

message("[process_single_site] site=", site_id, " month=", curr_month)

# output dirs
env_dir <- file.path("data", "drivers", "neon")
flux_dir <- file.path("data", "targets", "neon")
if (!dir.exists(env_dir)) dir.create(env_dir, recursive = TRUE)
if (!dir.exists(flux_dir)) dir.create(flux_dir, recursive = TRUE)

### Define some auxiliary functions
weighted_mean_sd <- function(input_compute) {
  ### Purpose: this function computes the weighted mean of flux and flux_err.
  # set default vals
  out_val <- tibble(mean = NA,err = NA)
  
  # Combine these as a data table, remove NA for each measurement pair
  input <- input_compute |>
    select(flux,flux_err) |>
    drop_na()
  
  if(nrow(input > 0)) {
    
    weights <- 1/(input$flux_err)^2
    vals <- input$flux
    
    mean <- sum(weights*vals)/sum(weights)
    err <- sqrt(1/sum(weights))
    
    out_val <- tibble(mean,err)
  }
  
  return(out_val)
  
}


flux_daily_agg <- function(input_fluxes) {
  ## The input is a nested data table at each location and halfhour.  We need to average across the methods and sites.
  
  # Globbed from bigleaf R package scripts
  # https://www.swissfluxnet.ethz.ch/index.php/documentation/conversion-sequences/
  # Go to umol CO2 m-2 s-1 → g C m-2 30min-1
  # molar mass of carbon (kg mol-1) = 0.012011
  # conversion micromole (umol) to mole (mol) = 1e-06
  # conversion kilogram (kg) to gram (g) = 1000
  # seconds per half hour = 1800
  
  conv <- 0.02161926
  
  # First compute the average across all flux methods at the startDateTime and the horizontalPosition
  
  avg_by_time_hor <- input_fluxes |> 
    mutate(flux = map(.x=flux_compute,.f=~weighted_mean_sd(.x)) ) |> 
    select(startDateTime,horizontalPosition,flux) |>
    unnest(cols=c(flux)) |>
    ungroup() |>
    rename(flux = mean,
           flux_err = err)
  
  
  ### Next Group by start date time
  avg_by_time <- avg_by_time_hor |>
    group_by(startDateTime) |>
    nest() |>
    mutate(flux = map(.x=data,.f=~weighted_mean_sd(.x)) ) |>
    select(startDateTime,flux) |>
    unnest(cols=c(flux)) |>
    ungroup() |>
    rename(flux = mean,
           flux_err = err) |>
    mutate(flux = zoo::na.approx(flux),  #Use zoo package to do linear interpolation for any NAs:
           flux_err = zoo::na.approx(flux_err))
  
  
  ### Now aggregate up to daily values
  avg_day <- avg_by_time |>
    mutate(day = floor_date(startDateTime,unit="day"),
           flux = flux*conv,
           flux_err = flux*conv) |>
    group_by(day) |>
    summarize(flux = sum(flux,na.rm=TRUE),
              flux_err = sqrt(sum(flux_err^2,na.rm=TRUE))  # Standard flux propogation
    )
  
  return(avg_day)
}




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
  flux_input <- compute_neon_flux(
    input_site_env = out_env_data$site_data,
    input_site_megapit = out_env_data$site_megapit
  )
  
  flux_full <- flux_daily_agg(flux_input) |>
    ungroup() |>
    mutate(site_id = site_id) |>
    relocate(site_id) |>
    rename(startDateTime = day)
  
  flux_out_file <- file.path(flux_dir, paste0("forecast_prediction-", site_id, "-", curr_month, ".csv"))
  readr::write_csv(flux_full, flux_out_file)
  message("[process_single_site] wrote flux: ", flux_out_file)
}, silent = FALSE)

message("[process_single_site] finished")