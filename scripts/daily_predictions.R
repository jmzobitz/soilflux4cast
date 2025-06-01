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
driver_file <- paste0("data/drivers/forecast/forecast_soil_drivers-",input_date,".csv")
observed_file <- "data/drivers/observed/observed_soil_drivers.csv"

# Read the existing data (create a new data frame if file doesn't exist)
if (file.exists(observed_file)) {
  current_observed <- read_csv(observed_file, show_col_types = FALSE)
} else {
  current_observed <- tibble()
}


# Acquire forecast data for the previous day across all horizons
input_drivers <- acquire_forecast(input_date) |>
  mutate(date = input_date) |>
  pivot_wider() |>
  relocate(date, field_site_id) |>
  rename(site = field_site_id)

# Write updated data back to CSV
write_csv(input_drivers, file = driver_file)

# Filter on the new observed data only
new_observed <- input_drivers |>
  filter(horizon == "f000",
         forecast == "gec00") |>
  mutate(date = input_date) |>
  pivot_wider() |>
  relocate(date, field_site_id) |>
  rename(site = field_site_id)


# Append new data to existing data
out_observed <- rbind(current_observed, new_observed) |>
  arrange(date,site) |> # Arrange by date and site alphabetically
  distinct()  # Remove any duplicate rows just in case

# Write updated data back to CSV
write_csv(out_observed, file = observed_file)


# Optional message
message("Forecast data for ", input_date, " appended to ", driver_file, " and ", observed_file)
