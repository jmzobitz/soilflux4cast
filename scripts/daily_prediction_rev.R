# Load required packages
library(readr)
library(dplyr)
library(tidyr)
library(stringr)

args <- commandArgs(trailingOnly = TRUE)
forecast_date <- args[1]  



# Acquire the NEON site_data 
site_data <- readr::read_csv(paste0("https://raw.githubusercontent.com/eco4cast/neon4cast-targets/","main/NEON_Field_Site_Metadata_20220412.csv"),show_col_types = FALSE) |> 
  dplyr::filter(terrestrial == 1)|> 
  dplyr::select(field_site_id,field_latitude,field_longitude)



# Define null model to create soil values
null_model <- function(TSOIL) {
  
  # Globbed from bigleaf R package scripts
  # molar mass of carbon (kg mol-1) = 0.012011
  # conversion micromole (umol) to mole (mol) = 1e-06
  # conversion kilogram (kg) to gram (g) = 1000
  # seconds per day = 86400
  
  conv <- 1e-6 * 0.012011 * 1000 * 86400
  
  R_S <- (0.598 + exp(0.044*(TSOIL-273.15)))*conv  # Convert to gC m-2 d-1
  
  return(R_S)
}

bucket <- "bio230014-bucket01"
path <- "neon4cast-drivers/noaa/gefs-v12/stage1"
endpoint <- "https://sdsc.osn.xsede.org"

stage1 <- 
  glue::glue("{bucket}/{path}/reference_datetime={forecast_date}") |>
  arrow::s3_bucket(endpoint_override = endpoint, anonymous = TRUE) |>
  arrow::open_dataset()


env_vars <- c("PRES","TSOIL", "SOILW", "WEASD", "SNOD","ICETK")

# Grab the data we need
forecast_data <- stage1 |> 
  filter(variable %in% env_vars,
         site_id %in% site_data$field_site_id) |> 
  collect() |>
  mutate(prediction = if_else(prediction ==9999.00,NA,prediction)) |>
  pivot_wider(names_from = "variable",values_from = "prediction")


# Compute forecast
input_forecast <- forecast_data |>
  mutate(model = 'null',
         value = null_model(TSOIL)) |>
  select(-all_of(env_vars))

forecast_file <- paste0('data/outputs/forecast_prediction-',forecast_date,'.csv')


# Write updated data back to CSV
write_csv(input_forecast, file = forecast_file)


# Optional message
message("Forecast prediction for ", forecast_date, " appended to ", forecast_file)
