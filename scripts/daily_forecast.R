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
driver_file <- "data/drivers/observed_soil_drivers.csv"

# Read the existing data (create a new data frame if file doesn't exist)
if (file.exists(driver_file)) {
  current_drivers <- read_csv(driver_file, show_col_types = FALSE)
} else {
  current_drivers <- tibble()
}

# Acquire forecast data for the previous day
input_drivers <- acquire_forecast(input_date, forecast_vals = "gec00", forecast_horizon = "f000") |>
  mutate(date = input_date) |>
  pivot_wider() |>
  relocate(date, field_site_id) |>
  rename(site = field_site_id)

# Append new data to existing data
out_drivers <- rbind(current_drivers, input_drivers) |>
  arrange(date,site) |> # Arrange by date and site alphabetically
  distinct()  # Remove any duplicate rows just in case

# Write updated data back to CSV
write_csv(out_drivers, file = driver_file)

# Optional message
message("Forecast data for ", input_date, " appended to ", driver_file)
