# Script to acquire new NEON data, save to file.

### Author: JMZ
### Last revised: 05-31-25
### Purpose: Code to acquire NEON data from select sites, save it to the
### working directory. Can run to catch up from data that we haven't collected yet.

args <- commandArgs(trailingOnly = TRUE)

curr_month <- as.character(args[1])


# Load libraries
library(neonSoilFlux)
library(tidyverse)
library(neonUtilities)


### Input will be a month that we provide in YYYY-MM format as a string


# Acquire the NEON site_data
site_names <- readr::read_csv(paste0("https://raw.githubusercontent.com/eco4cast/neon4cast-targets/", "main/NEON_Field_Site_Metadata_20220412.csv"), show_col_types = FALSE) |>
  dplyr::filter(terrestrial == 1) |>
  dplyr::select(field_site_id, field_latitude, field_longitude) |>
  dplyr::rename(site_id = field_site_id)


# Define a helper function to compute the mean:
compute_mean <- function(input_data) {
  n_good <- sum(!is.na(input_data$flux))
  n_tot <- nrow(input_data)
  out_tibble <- tibble(
    flux = NA,
    flux_err = NA,
    n = 0
  )

  if (n_good > 0) {
    out_tibble <- input_data |>
      filter(!is.na(flux_err)) |>
      summarize(
        flux = mean(flux, na.rm = TRUE),
        flux_err = neonSoilFlux::quadrature_error(1 / n(), flux_err),
        n = n(),
        .groups = "drop"
      )
  }



  return(out_tibble)
}


# for(i in 1:nrow(site_names)) {
for (i in 1:3) {
  # Process
  try(
    # NOTE: you will need to say y/n at several points here
    {
      curr_site_name <- site_names$site_id[[i]]
      print(curr_site_name)
      start_time <- Sys.time()
      # Do a quick check to see if we have env data already downloaded
      #   if(!file.exists(curr_env_name)) {

      out_env_data <- acquire_neon_data(
        site_name = curr_site_name,
        download_date = curr_month,
        provisional = TRUE
      )

      VSWC_data <- out_env_data$site_data$data[[2]] |>
        filter(
          verticalPosition == "501",
          VSWCFinalQF == 0,
          hour(startDateTime) == 0,
          minute(startDateTime) == 0
        ) |>
        group_by(startDateTime) |>
        summarize(VSWC = mean(VSWCMean, na.rm = TRUE))


      SOILT_data <- out_env_data$site_data$data[[3]] |>
        filter(
          verticalPosition == "501",
          soilTempFinalQF == 0,
          hour(startDateTime) == 0,
          minute(startDateTime) == 0
        ) |>
        group_by(startDateTime) |>
        summarize(SOILT = mean(soilTempMean, na.rm = TRUE))

      # Do a full join because we may have NAs in both
      env_joined <- SOILT_data |>
        full_join(VSWC_data, by = "startDateTime")

      # Process
      try(
        # NOTE: you will need to say y/n at several points here
        {
          out_driver_name <- paste0("data/drivers/observed/neon/soil_drivers-", curr_site_name, "-", curr_month, ".csv")
          write_csv(env_joined, file = out_driver_name)
        }
      )

      # Process
      try(
        # NOTE: you will need to say y/n at several points here
        {
          flux_full <- compute_neon_flux(
            input_site_env = out_env_data$site_data,
            input_site_megapit = out_env_data$site_megapit
          ) |>
            select(-surface_diffusivity) |>
            unnest(cols = c(flux_compute)) |>
            filter(
              hour(startDateTime) == 0,
              minute(startDateTime) == 0
            ) |>
            group_by(startDateTime) |>
            nest() |>
            mutate(fluxes = map(data, compute_mean)) |>
            select(-data) |>
            unnest(cols = c(fluxes))



          out_flux_name <- paste0("data/outputs/neon/forecast_prediction-", curr_site_name, "-", curr_month, ".csv")

          write_csv(flux_full, file = out_flux_name)
        }
      )
    }
  )
}
