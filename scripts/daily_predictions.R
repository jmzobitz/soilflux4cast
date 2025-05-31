# Load required packages
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)

# Source the forecast function (make sure the path is correct)
source("R/acquire_forecast.R")

# Get the previous day's date in YYYY-MM-DD format
input_date <- as.character(Sys.Date() - 1)

# File path to the driver data
driver_file <- paste0("data/drivers/forecast_soil_drivers-",input_date,".csv")


# Acquire forecast data for the previous day across all horizons
input_drivers <- acquire_forecast(input_date) |>
  mutate(date = input_date) |>
  pivot_wider() |>
  relocate(date, field_site_id) |>
  rename(site = field_site_id)


# Write updated data back to CSV
write_csv(input_drivers, file = driver_file)

# Optional message
message("Forecast data for ", input_date, " appended to ", driver_file)
