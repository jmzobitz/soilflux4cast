# Load required packages
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(xgboost)
library(lubridate)
library(tidyverse)
library(broom)

### Source some functions
source("R/download_annual_values.R")
source("R/fit_exp_soilR.R")
source('R/drivers_available.R')
source('R/targets_available.R')

### Function that summarizes a forecast ensemble
forecast_summarize_horizon <- function(input_forecast,fx_date) {
  
  input_forecast |>
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
    mutate(horizon = as.numeric(day-as.POSIXct.Date(ymd(fx_date)),unit="days"))
  
}

# Process a single site (env + flux). Usage: Rscript scripts/daily_prediction_horizon.R YYYY-MM-DD

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
  #filter(horizon < 86400) |>   # Get up to the day predictions
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
  
  conv <- 1e-6 * 0.012011 * 1000 * 86400   # avoid this for now
  
  R_S <- (0.598 + exp(0.044*(TSOIL-273.15)))  #*conv  # Convert to gC m-2 d-1
  
  return(R_S)
}


# Compute forecast
input_forecast_null <- driver_data |>
  mutate(model = 'null',
         value = null_model(TSOIL)) |>
  select(-all_of(env_vars)) |>
  forecast_summarize_horizon(fx_date = forecast_date)

### Model 3a: linear model using all available data, by site
### Now do a linear model using all data


# Collect all the known targets and drivers
drivers <- drivers_available()
targets <- targets_available()

# Now join the two together, by siteID and date, nesting them by site

joined_vars <- targets |>
  inner_join(drivers, by=c("site_id","startDateTime"="datetime")) |> 
  drop_na() |>
  mutate(TSOIL = TSOIL - 273.15) |>
  group_by(site_id) |>
  nest()



# Now mutate and do a linear model
fit_vals <- joined_vars |>
  mutate(lm_fit = map(.x=data,.f=~lm(flux~SOILW+TSOIL,data=.x)),
         coeff = map(.x=lm_fit,.f=~(.x |> broom::tidy())),
         sigma = map_dbl(.x=lm_fit,.f=~(sd(.x$residuals)))
  ) |>
  select(site_id,coeff,sigma)


update_lm_model <- function(input_coeff,sigma_ps,SOILW,TSOIL,unc_fac=1) {
  
  n_ps <- length(sigma_ps)
  
  # New dataset with new covariates
  newdat <- tibble(
    SOILW,
    TSOIL
  )
  
  coeff <- input_coeff |>
    mutate(estimate = map2_dbl(.x=estimate,.y=std.error,.f=~(.x+rnorm(1,mean=0,sd = unc_fac*.y)) ) ) |>
    select(term,estimate)
  
  # Convert to named vector
  coef_vec <- deframe(coeff)
  
  # Create prediction matrix (include intercept by adding a column of 1)
  X <- model.matrix(~ 1 + SOILW + TSOIL, data = newdat)
  
  # Predictions
  pred <- (X %*% coef_vec) + rnorm(n_ps,mean=0,sd=sigma_ps)  |>
    as.numeric()
  
  return(pred)
  
  

  
  
}

# Compute forecast
input_forecast_lm <- driver_data |>
  mutate(TSOIL = TSOIL - 273.15) |>
  inner_join(fit_vals,by="site_id") |>
  mutate(model = 'lm',
         value = pmap_dbl(.l=list(coeff,sigma,SOILW,TSOIL),.f=~update_lm_model(..1,..2,..3,..4)) ) |>
  select(-all_of(env_vars),-coeff,-sigma)  |>
  forecast_summarize_horizon(fx_date = forecast_date)

####


### Model 3c: Fit an exponential model to all data from the year before

drivers <- download_annual_values("drivers",year_before)
targets <- download_annual_values("targets",year_before)

# now join by site and day

joined_vars <- targets |>
  inner_join(drivers,by=c("site_id","startDateTime"="datetime")) |>
  mutate(TSOIL10 = (TSOIL - 273.15 - 10)/10,
         ln_flux = log(flux),
         ln_ok = ((!is.na(ln_flux) & is.finite(ln_flux) & 
                     !is.na(TSOIL10)))
         ) |>
  filter(ln_ok) |>
  group_by(site_id) |>
  nest() |>
  mutate(n_obs = map_dbl(data,nrow)) |>
  filter(n_obs > 2) |>
  select(-n_obs)


# Now mutate and do a linear model
fit_vals <- joined_vars |>
  mutate(lm_fit = map(.x=data,.f=~lm(ln_flux~TSOIL10,data=.x)),
         coeff = map(.x=lm_fit,.f=~(.x |> broom::tidy())),
         sigma = map_dbl(.x=lm_fit,.f=~(sd(.x$residuals)))
  ) |>
  select(site_id,coeff,sigma)


update_lm_model <- function(input_coeff,sigma_ps,TSOIL,unc_fac=1) {
  
  n_ps <- length(sigma_ps)
  
  # New dataset with new covariates
  newdat <- tibble(
    TSOIL
  )
  
  coeff <- input_coeff |>
    mutate(estimate = map2_dbl(.x=estimate,.y=std.error,.f=~(.x+rnorm(1,mean=0,sd = unc_fac*.y)) ) ) |>
    select(term,estimate)
  
  # Convert to named vector
  coef_vec <- deframe(coeff)
  
  # Create prediction matrix (include intercept by adding a column of 1)
  X <- model.matrix(~ 1 + TSOIL, data = newdat)
  
  # Predictions
  pred <- (X %*% coef_vec) + rnorm(n_ps,mean=0,sd=sigma_ps)  |>
    as.numeric()
  
  return(pred)
  
  
}

# Compute forecast
input_forecast_exp <- driver_data |>
  mutate(TSOIL10 = (TSOIL - 273.15-10)/10 ) |>
  inner_join(fit_vals,by="site_id") |>
  mutate(model = 'exp',
         value = pmap_dbl(.l=list(coeff,sigma,TSOIL10),.f=~update_lm_model(..1,..2,..3) |> exp()) ) |>   # convert back to exponential
  select(-all_of(env_vars),-coeff,-sigma,-TSOIL10)  |>
  forecast_summarize_horizon(fx_date = forecast_date)



## Models 3d and 3e: Get drivers and targets from the previous data drop

drivers_month <- download_annual_values("drivers",curr_year,month = curr_month)
targets_month <- download_annual_values("targets",curr_year,month = curr_month)
# Acquire and download the model and

joined_vars_month <- targets_month |>
  inner_join(drivers_month,by=c("site_id","startDateTime"="datetime")) |>
  mutate(TSOIL10 = (TSOIL - 273.15-10)/10,
         TSOIL = TSOIL - 273.15,
         ln_flux = log(flux),
         ln_ok = ((!is.na(ln_flux) & is.finite(ln_flux) & 
                     !is.na(TSOIL10)))
  ) |>
  filter(ln_ok) |>
  group_by(site_id) |>
  nest() |>
  mutate(n_obs = map_dbl(data,nrow)) |>
  filter(n_obs > 2) |>
  select(-n_obs)


# Now mutate and do a linear model
fit_vals_month <- joined_vars_month |>
  mutate(lm_fit = map(.x=data,.f=~lm(ln_flux~TSOIL10,data=.x)),
         coeff = map(.x=lm_fit,.f=~(.x |> broom::tidy())),
         sigma = map_dbl(.x=lm_fit,.f=~(sd(.x$residuals)))
  ) |>
  select(site_id,coeff,sigma)


update_lm_model <- function(input_coeff,sigma_ps,TSOIL,unc_fac=1) {
  
  n_ps <- length(sigma_ps)
  
  # New dataset with new covariates
  newdat <- tibble(
    TSOIL
  )
  
  coeff <- input_coeff |>
    mutate(estimate = map2_dbl(.x=estimate,.y=std.error,.f=~(.x+rnorm(1,mean=0,sd = unc_fac*.y)) ) ) |>
    select(term,estimate)
  
  # Convert to named vector
  coef_vec <- deframe(coeff)
  
  # Create prediction matrix (include intercept by adding a column of 1)
  X <- model.matrix(~ 1 + TSOIL, data = newdat)
  
  # Predictions
  pred <- (X %*% coef_vec) + rnorm(n_ps,mean=0,sd=sigma_ps)  |>
    as.numeric()
  
  return(pred)
  
  
}

# Compute forecast
input_forecast_exp_month <- driver_data |>
  mutate(TSOIL10 = (TSOIL - 273.15-10)/10 ) |>
  inner_join(fit_vals_month,by="site_id") |>
  mutate(model = 'exp_month',
         value = pmap_dbl(.l=list(coeff,sigma,TSOIL10),.f=~update_lm_model(..1,..2,..3) |> exp()) ) |>   # convert back to exponential
  select(-all_of(env_vars),-coeff,-sigma,-TSOIL10)  |>
  forecast_summarize_horizon(fx_date = forecast_date)



## Model 3e - xgboost!

# input_param_data is what we use to parameterize the model
# input_driver_data is what we use to evaluate the model
# computes process ID - no parameter uncertainty
compute_xgboost <- function(input_param_data,input_eval_data) {
  
  input_param_data <- input_param_data |> drop_na()
  
  if(nrow(input_param_data) > 5)  {
    
    # Convert to matrix
    X <- input_param_data |> 
      select(TSOIL,SOILW) |>
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
    
    sigma_ps <- sd(getinfo(dtrain, "label") - predict(model, dtrain))

    
    eval_matrix <- input_eval_data |>
      select(TSOIL,SOILW) |>
      as.matrix()
    
    # Predictions
    input_eval_data$value <- predict(model, eval_matrix) + rnorm(1,mean=0,sd=sigma_ps)
    
    
    
    
  } else {
    input_eval_data$value <- NA
  }
  
  
  return(input_eval_data)
  
  
}

# now join by site and day
eval_data <- driver_data |>
  group_by(site_id) |>
  nest() |>
  rename(eval_data = data)

# Now ready to map!
joined_vars_month <- targets_month |>
  inner_join(drivers_month,by=c("site_id","startDateTime"="datetime")) |>
  mutate(
         TSOIL = TSOIL - 273.15
         ) |>
  group_by(site_id) |>
  nest() |>
  mutate(n_obs = map_dbl(data,nrow)) |>
  filter(n_obs > 5) |>
  select(-n_obs) |>
  rename(param_data = data) |>
  inner_join(eval_data,by="site_id")


input_forecast_xg <- joined_vars_month  |>
  mutate(fit_value = map2(.x=param_data,.y=eval_data,.f=~compute_xgboost(.x,.y))) |>
  select(site_id,fit_value) |>
  unnest(cols=c(fit_value)) |>
  mutate(model = 'xgboost') |>
  select(-all_of(env_vars))  |>
  forecast_summarize_horizon(fx_date = forecast_date)


forecast_file <- paste0('data/outputs/forecast_prediction_horizon-',forecast_date,'.csv')

# glob forecasts together - we assume that we average across the day
input_forecast_horizon <- rbind(input_forecast_lm,
                        input_forecast_exp,
                        input_forecast_exp_month,
                        input_forecast_xg
) 

# Write updated data back to CSV
write_csv(input_forecast_horizon, file = forecast_file)


# Optional message
message("Forecast horizon prediction for ", forecast_date, " appended to ", forecast_file)
