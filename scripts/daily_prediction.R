# Load required packages
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(xgboost)
library(lubridate)
library(tidyverse)

### Source some functions
source("R/download_annual_values.R")
source("R/fit_exp_soilR.R")
source('R/drivers_available.R')
source('R/targets_available.R')


args <- commandArgs(trailingOnly = TRUE)
forecast_date <- args[1]  

### STEP 1: Acquire the NEON site_data 
site_data <- readr::read_csv(paste0("https://raw.githubusercontent.com/eco4cast/neon4cast-targets/","main/NEON_Field_Site_Metadata_20220412.csv"),show_col_types = FALSE) |> 
  dplyr::filter(terrestrial == 1)|> 
  dplyr::select(field_site_id,field_latitude,field_longitude)

bucket <- "bio230014-bucket01"
path <- "neon4cast-drivers/noaa/gefs-v12/stage1"
endpoint <- "https://sdsc.osn.xsede.org"

stage1 <- 
  glue::glue("{bucket}/{path}/reference_datetime={forecast_date}") |>
  arrow::s3_bucket(endpoint_override = endpoint, anonymous = TRUE) |>
  arrow::open_dataset()


env_vars <- c("PRES","TSOIL", "SOILW", "WEASD", "SNOD","ICETK")

# Grab the data we need
driver_data <- stage1 |> 
  filter(variable %in% env_vars,
         site_id %in% site_data$field_site_id) |> 
  collect() |>
  mutate(prediction = if_else(prediction ==9999.00,NA,prediction)) |>
  pivot_wider(names_from = "variable",values_from = "prediction")


# Remove any temporary files
terra::tmpFiles(remove = TRUE)
unlink(list.files(tempdir(), full.names = TRUE), recursive = TRUE)


### STEP 2: determine the month and year for which we data to parameterize models (used for models 4 and 5)

curr_month <- driver_data$datetime |>
  min() |>
  month()

curr_year <- driver_data$datetime |>
  min() |>
  year()

curr_day <- driver_data$datetime |>
  min() |>
  day()

if(curr_day < 15) {
  curr_month <- curr_month - 2 # Go two months back
} else {
  curr_month <- curr_month - 1
}

if (curr_month == 0) {
  curr_month <- 12
  curr_year <- curr_year - 1
}

if (curr_month == -1) {
  curr_month <- 11
  curr_year <- curr_year - 1
}

year_before <- curr_year-1


### STEP 3: Start modeling!
### Model 3a: Define null model to create soil values
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


# Compute forecast
input_forecast_null <- driver_data |>
  mutate(model = 'null',
         value = null_model(TSOIL)) |>
  select(-all_of(env_vars))

### Model 3a: linear model using all available data, by site
### Now do a linear model using all data


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
input_forecast_lm <- driver_data |>
  inner_join(fit_vals,by="site_id") |>
  mutate(model = 'lm',
         value = pmap_dbl(.l=list(lm_fit,TSOIL,SOILW),.f=~predict(..1,tibble(TSOIL=..2-273.15,SOILW=..3)) ) ) |>
  select(-all_of(env_vars),-lm_fit,-coeff)

####


### Model 3c: Fit an exponential model to all data from the year before

drivers <- download_annual_values("drivers",year_before)
targets <- download_annual_values("targets",year_before)

# now join by site and day

joined_data <- drivers |>
  inner_join(targets,by=c("site_id","startDateTime")) |>
  group_by(site_id) |>
  nest() |>
  mutate(fit_coeff = map(data,fit_exp_soilR)) |>
  select(-data)

calc_soilR <- function(params,data) {
  
  TSOIL <- data$TSOIL
  kR <- params |> filter(term == "kR") |> pull(value)
  Q10 <- params |> filter(term == "Q10") |> pull(value)
  
  data <- data |>
    mutate(value = kR*Q10^((TSOIL-10))/10)
  
  return(data)
  
}

input_forecast_exp <- driver_data |>
  mutate(TSOIL = TSOIL-273.15) |>
  group_by(site_id) |>
  nest() |>
  inner_join(joined_data,by="site_id") |>
  mutate(fit_value = map2(.x=fit_coeff,.y=data,.f=~calc_soilR(.x,.y))) |>
  select(site_id,fit_value) |>
  unnest(cols=c(fit_value)) |>
  mutate(model = 'exp') |>
  select(-all_of(env_vars))
  
## Models 3d and 3e: Get drivers and targets from the previous data drop

drivers_month <- download_annual_values("drivers",curr_year,month = curr_month)
targets_month <- download_annual_values("targets",curr_year,month = curr_month)
# Acquire and download the model and

joined_data_month <- drivers_month |>
  inner_join(targets_month,by=c("site_id","startDateTime")) |>
  rename(TSOIL = SOILT,
         SOILW = VSWC) |>
  group_by(site_id) |>
  nest() |>
  rename(param_data = data)

# We can use this for each month - shared between each model
nested_model_info <- driver_data |>
  mutate(TSOIL = TSOIL-273.15) |>
  group_by(site_id) |>
  nest() |>
  rename(eval_data = data) |>
  inner_join(joined_data_month,by="site_id")


# # model 3d: exponential model for a given month
# 
 input_forecast_exp_month <- nested_model_info |>
   mutate(
     param_data_temp = map(.x=param_data,.f=~(.x |> rename(SOILT=TSOIL) ) ),
     fit_coeff = map(param_data_temp,fit_exp_soilR)) |>
   select(-param_data_temp) |>
   mutate(fit_value = map2(.x=fit_coeff,.y=eval_data,.f=~calc_soilR(.x,.y))) |>
   select(site_id,fit_value) |>
   unnest(cols=c(fit_value)) |>
   mutate(model = 'exp_month') |>
   select(-all_of(env_vars))

## Model 3f - xgboost!

# input_param_data is what we use to parameterize the model
# input_driver_data is what we use to evaluate the model
compute_xgboost <- function(input_param_data,input_eval_data) {
  
  # Convert to matrix
  X <- input_param_data |> select(TSOIL,SOILW) |>
    as.matrix()
  
  Y <- input_param_data |> pull(flux)
  
  
  dtrain <- xgb.DMatrix(data = X, label = Y)
  
  model <- xgboost(
    data = dtrain,
    objective = "reg:squarederror",
    max_depth = 3,
    eta = 0.1,
    nrounds = 200,
    verbose = 0
  )
  
  
  eval_matrix <- input_eval_data |>
    select(TSOIL,SOILW) |>
    as.matrix()
  
  # Predictions
  input_eval_data$value <- predict(model, eval_matrix)
  
  return(input_eval_data)
  
  
}

# now join by site and day

# Now ready to map!

input_forecast_xg <- nested_model_info |>
  mutate(fit_value = map2(.x=param_data,.y=eval_data,.f=~compute_xgboost(.x,.y))) |>
  select(site_id,fit_value) |>
  unnest(cols=c(fit_value)) |>
  mutate(model = 'xgboost') |>
  select(-all_of(env_vars))


forecast_file <- paste0('data/outputs/forecast_prediction-',forecast_date,'.csv')

# glob forecasts together - we assume that we average across the day
input_forecast <- rbind(input_forecast_null,
                        input_forecast_lm,
                        input_forecast_exp,
                        input_forecast_exp_month,
                        input_forecast_xg
                        ) |>
  mutate(day = floor_date(datetime,unit = "day")) |>
  group_by(site_id,model,day) |>
  dplyr::reframe(value =
                   stats::quantile(value,na.rm=TRUE,probs = c(0.025,0.10,0.5,0.9,.975),
                   ),
                 name = c("q0.025", "q0.10","q0.5", "q0.90","q0.975"),
                 mean = mean(value, na.rm = TRUE),
                 sd = sd(value, na.rm = TRUE)
  ) |> 
  tidyr::pivot_wider() |>
  ungroup() |>
  filter(day == as.Date(forecast_date))  # Just get the forecasts from that day


# Write updated data back to CSV
write_csv(input_forecast, file = forecast_file)


# Optional message
message("Forecast prediction for ", forecast_date, " appended to ", forecast_file)
