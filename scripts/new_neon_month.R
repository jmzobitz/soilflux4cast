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

# Globbed from bigleaf R package scripts
# molar mass of carbon (kg mol-1) = 0.012011
# conversion micromole (umol) to mole (mol) = 1e-06
# conversion kilogram (kg) to gram (g) = 1000
# seconds per half hour = 1800

conv <- 1e-6 * 0.012011 * 1000 * 1800

for(i in 1:nrow(site_names)) {
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
          out_driver_name <- paste0("data/drivers/neon/soil_drivers-", curr_site_name, "-", curr_month, ".csv")
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
            group_by(startDateTime) |>
            mutate(flux =flux*conv,
                   flux_err = flux_err*conv,
            ) |>
            summarize(flux_out = mean(flux,na.rm=TRUE),
                      flux_err = sd(flux,na.rm=TRUE)/sqrt(sum(!is.na(flux)))
                      
            ) |>
            rename(flux = flux_out) |>
            group_by(startDateTime = floor_date(startDateTime,unit="day")) |>
            summarize(flux = sum(flux,na.rm=TRUE),
                      flux_err = sqrt(sum(flux_err^2,na.rm=TRUE))) |>
            ungroup() |>
            mutate(site_id = curr_site_name) |>
            relocate(site_id) |>
            arrange(startDateTime)



          out_flux_name <- paste0("data/targets/neon/forecast_prediction-", curr_site_name, "-", curr_month, ".csv")

          write_csv(flux_full, file = out_flux_name)
        }
      )
    }
  )
}

# Now glob together all the env values for a given month and delete the intermediate files:

env_files <- list.files(path = "data/drivers/neon",
                        full.names = TRUE
                        ) |>
  str_subset(pattern = paste0("(?<=[:alpha:]{4}-)", curr_month, ".csv"))


if(length(env_files)>0) {
  out_env <- tibble(
    site_id = str_extract(env_files,pattern = "(?<=soil_drivers-)[:alpha:]{4}"),
    values = lapply(env_files, readr::read_csv)
  ) |>
    unnest(cols=c(values))
  
   env_save_file <- paste0("data/drivers/neon_soil_drivers-", curr_month, ".csv")
   
   # Save globbed file
   write_csv(out_env, file = env_save_file)
   
   # Remove the separate files
   file.remove(env_files)
   
}

# Do the same for the targets (soil flux)
flux_files <- list.files(path = "data/targets/neon", 
                         full.names = TRUE
                         ) |>
  str_subset(pattern = paste0("(?<=[:alpha:]{4}-)", curr_month, ".csv"))



if(length(flux_files)>0) {
  out_flux <- tibble(
    site_id = str_extract(flux_files,pattern = "(?<=forecast_prediction-)[:alpha:]{4}"),
    values = lapply(flux_files, readr::read_csv)
  ) |>
    unnest(cols=c(values))
  
  flux_save_file <- paste0("data/targets/neon_targets-", curr_month, ".csv")
  
  # Save globbed file
  write_csv(out_flux, file = flux_save_file)
  
  # Remove the separate files
  file.remove(flux_files)
  
}


