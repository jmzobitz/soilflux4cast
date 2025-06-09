# Load required packages
library(readr)
library(purrr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)

args <- commandArgs(trailingOnly = TRUE)
input_file <- args[1]  


# Define null model to create soil values
null_model <- function(TSOIL) {
  
  R_S <- (0.598 + exp(0.044*(TSOIL-273.15)))
  
  return(R_S)
}

# Compute forecast
input_forecast <- readr::read_csv(input_file) |>
  mutate(model = 'null',
         value = null_model(TSOIL)) |>
  select(date,site,forecast,horizon,model,value)


# Determine the date of forecast
forecast_date <- input_file |> str_extract(pattern = "[:digit:]{4}-[:digit:]{2}-[:digit:]{2}(?=.csv)")

forecast_file <- paste0('data/outputs/forecast_prediction-',forecast_date)


# Write updated data back to CSV
write_csv(input_forecast, file = forecast_file)


# Optional message
message("Forecast prediction for ", forecast_date, " appended to ", forecast_file)
