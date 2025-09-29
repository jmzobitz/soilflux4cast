# Load required packages
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

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
input_forecast_null <- forecast_data |>
  mutate(model = 'null',
         value = null_model(TSOIL)) |>
  select(-all_of(env_vars))


### Now do a linear model based on last month's data
source('R/drivers_available.R')
source('R/targets_available.R')

# Collect all the known targets and drivers
drivers <- drivers_available()
targets <- targets_available()

# Now join the two together, by siteID and date, nesting them by site

joined_vars <- targets |>
  inner_join(drivers, by=c("site_id","startDateTime")) |> 
  drop_na() |>
  rename(SOILW = VSWC,
         TSOIL = SOILT) |>
  group_by(site_id) |>
  nest()



# Now mutate and do a linear model
fit_vals <- joined_vars |>
  mutate(lm_fit = map(.x=data,.f=~lm(flux~SOILW+TSOIL,data=.x)),
         coeff = map(.x=lm_fit,.f=~(.x$coefficients)),
         sigma = map_dbl(.x=lm_fit,.f=~(sd(.x$residuals)))
  ) |>
  select(site_id,lm_fit,coeff)



# Compute forecast
input_forecast_lm <- forecast_data |>
  inner_join(fit_vals,by="site_id") |>
  mutate(model = 'lm',
         value = pmap_dbl(.l=list(lm_fit,TSOIL,SOILW),.f=~predict(..1,tibble(TSOIL=..2-273.15,SOILW=..3)) ) ) |>
  select(-all_of(env_vars),-lm_fit,-coeff)

####

forecast_file <- paste0('data/outputs/forecast_prediction-',forecast_date,'.csv')

# glob forecasts together
input_forecast <- rbind(input_forecast_null,input_forecast_lm)

# Write updated data back to CSV
write_csv(input_forecast, file = forecast_file)


# Optional message
message("Forecast prediction for ", forecast_date, " appended to ", forecast_file)
