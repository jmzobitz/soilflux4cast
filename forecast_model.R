# For a given forecast date, build out the model for each site using last months data (when available), and then 
library(tidyverse)
library(rMR)
library(glue)
source("ignore_sigpipe.R")

source('R/gefs_soil_forecasts.R')

forecast_date <- Sys.Date() - months(1)

site_id <- "BART"

noaa_date <- Sys.Date() - days(3)  #Need to use yesterday's NOAA forecast because today's is not available yet

# Step 0: Define a unique name which will identify your model in the leaderboard and connect it to team members info, etc
model_id <- "soilflux4cast_example"

# Step 1: Download latest target data and site description data

curr_site <- "BART"

curr_forecast <- gefs_soil_forecasts(input_date = forecast_date,
                                     neon_site = curr_site
                                     ) |>
  filter(ensemble == "gec00",horizon ==0)


# Step 2: Get meterological predictions as drivers
df_past <- neon4cast::noaa_stage3()

## Helper function: for each site, average over predicted 0h horizon ensembles to get 'historic values'
noaa_mean_historical <- function(df_past, site, var) {
  df_past |>
    dplyr::filter(site_id == site,
                  variable == var) |>
    dplyr::rename(ensemble = parameter) |>
    dplyr::select(datetime, prediction, ensemble) |>
    dplyr::mutate(date = as_date(datetime)) |>
    dplyr::group_by(date) |>
    dplyr::summarize(air_temperature = mean(prediction, na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::rename(datetime = date) |>
    dplyr::mutate(air_temperature = air_temperature - 273.15) |>
    dplyr::collect()
}

## Helper fn: get daily average temperature from each ensemble in future
noaa_mean_forecast <- function(site, var, reference_date) {
  endpoint = "data.ecoforecast.org"
  bucket <- glue::glue("neon4cast-drivers/noaa/gefs-v12/stage1/0/{reference_date}")
  s3 <- arrow::s3_bucket(bucket, endpoint_override = endpoint, anonymous = TRUE)
  
  # stage1 air temp is Celsius
  arrow::open_dataset(s3) |>
    dplyr::filter(site_id == site,
                  datetime >= lubridate::as_datetime(forecast_date),
                  variable == var) |>
    dplyr::select(datetime, prediction, parameter) |>
    dplyr::mutate(datetime = as_date(datetime)) |>
    dplyr::group_by(datetime, parameter) |>
    dplyr::summarize(air_temperature = mean(prediction), .groups = "drop") |>
    dplyr::select(datetime, air_temperature, parameter) |>
    dplyr::rename(ensemble = parameter) |>
    dplyr::collect()
  
}




#Step 3.0: Define the forecasts model for a site
forecast_site <- function(site) {
  message(paste0("Running site: ", site))
  
  # Get site information for elevation
  site_info <- site_data |> dplyr::filter(field_site_id == site)
  
  # historical temperatures
  noaa_past_mean <- noaa_mean_historical(df_past, site, "air_temperature")
  
  # Merge in past NOAA data into the targets file, matching by date.
  site_target <- target |>
    dplyr::select(datetime, site_id, variable, observation) |>
    dplyr::filter(variable %in% c("temperature", "oxygen"), 
                  site_id == site) |>
    tidyr::pivot_wider(names_from = "variable", values_from = "observation") |>
    dplyr::left_join(noaa_past_mean, by = c("datetime"))
  
  rm(noaa_past_mean) # save RAM 
  
  # Fit linear model based o # n past data: water temperature = m * air temperature + b
  fit <- lm(temperature ~ air_temperature, data = site_target)
  
  #  Get 30-day predicted temperature ensemble at the site
  noaa_future <- noaa_mean_forecast(site, "TMP", noaa_date)
  
  # use the linear model (predict.lm) to forecast water temperature for each ensemble member
  temperature <- 
    noaa_future |> 
    mutate(site_id = site,
           prediction = predict(fit, tibble(air_temperature)),
           variable = "temperature")
  
  # use forecasted water temperature to predict oxygen by assuming that oxygen is saturated.
  forecasted_oxygen <- 
    rMR::Eq.Ox.conc(temperature$prediction, 
                    elevation.m = site_info$field_mean_elevation_m,
                    bar.press = NULL,
                    bar.units = NULL,
                    out.DO.meas = "mg/L",
                    salinity = 0,
                    salinity.units = "pp.thou")
  # stick bits together                  
  oxygen <- 
    noaa_future |> 
    mutate(site_id = site,
           prediction = forecasted_oxygen,
           variable = "oxygen")
  
  forecast <- dplyr::bind_rows(temperature, oxygen)
  
  # Format results to EFI standard
  forecast <- forecast |>
    mutate(reference_datetime = forecast_date,
           family = "ensemble",
           model_id = model_id) |>
    rename(parameter = ensemble) |>
    select(model_id, datetime, reference_datetime,
           site_id, family, parameter, variable, prediction)
}

### AND HERE WE GO! We're ready to start forecasting ### 

## Test with a single site first!
forecast <- forecast_site( sites[1] )

#Visualize the ensemble predictions -- what do you think?
forecast |> 
  ggplot(aes(x = datetime, y = prediction, group = parameter)) +
  geom_line(alpha=0.3) +
  facet_wrap(~variable, scales = "free")


# Run all sites -- may be slow!
forecast <- map_dfr(sites, forecast_site)


#Forecast output file name in standards requires for Challenge.
# csv.gz means that it will be compressed
file_date <- Sys.Date() #forecast$reference_datetime[1]
forecast_file <- paste0("aquatics","-",file_date,"-",model_id,".csv.gz")

#Write csv to disk
write_csv(forecast, forecast_file)

# Step 4: Submit forecast!

neon4cast::submit(forecast_file = forecast_file, metadata = NULL, ask = FALSE)
