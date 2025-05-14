
library(tidyverse)
library(neon4cast)
library(lubridate)
library(rMR)
library(glue)
source("ignore_sigpipe.R")

#forecast_date <- Sys.Date()
#noaa_date <- Sys.Date() - days(3)  #Need to use yesterday's NOAA forecast because today's is not available yet

noaa_date <- "2022-06-01"
curr_site <- "BART"
vars <- c("TSOIL","SOILW")

# Step 0: Define a unique name which will identify your model in the leaderboard and connect it to team members info, etc
model_id <- "neon4cast_example"

# Step 1: Download latest target data and site description data
#target <- readr::read_csv(paste0("https://data.ecoforecast.org/neon4cast-targets/","aquatics/aquatics-targets.csv.gz"), #guess_max = 1e6)
site_data <- readr::read_csv(paste0("https://raw.githubusercontent.com/eco4cast/neon4cast-targets/","main/NEON_Field_Site_Metadata_20220412.csv")) |> 
  dplyr::filter(terrestrial == 1)

sites <- site_data$field_site_id

## Load up the sites where there is adequate flux data available
load('data/daily-test-flux.Rda')

testing_sites <- sites[sites %in% summarized_fluxes$site]

### These are the ones where we will need to build out a forecast for


### Helper fn: Get and summarize the historical forecast date
make_stage3 <- function(month,days,site,var) {
  
  date_seq <- seq(as.Date(paste0(month,"-01")), length.out=days, by="days") |> lubridate::as_datetime()
  
  my_results <- vector(mode="list",length = days)
  
  for (i in 1:days) {
    my_results[[i]] <- arrow::open_dataset(s3) |>
      dplyr::filter(site_id == site,
                    datetime == date_seq[[i]],
                    variable %in% var
      ) |>
      dplyr::select(datetime, prediction, variable) |>
      dplyr::group_by(datetime, variable) |>
      dplyr::summarize(prediction = mean(prediction), .groups = "drop") |>
      dplyr::collect()
  }
  
  my_results |> bind_rows()
}


historical_data <- vector(mode="list",length=length(testing_sites))

for (i in 1:length(testing_sites)) {
  print(testing_sites[[i]])
  historical_data[[i]] <- make_stage3("2022-05",31,testing_sites[[i]],vars)
}

## Helper fn: get daily temperature from each ensemble in future
noaa_ensemble_forecast <- function(site, input_var, reference_date) {
  bucket <- "bio230014-bucket01"
  path <- "neon4cast-drivers/noaa/gefs-v12/stage1"
  endpoint <- "https://sdsc.osn.xsede.org"
  
  bucket <- glue::glue("{bucket}/{path}/reference_datetime={reference_date}")
  s3 <- arrow::s3_bucket(bucket, endpoint_override = endpoint, anonymous = TRUE)
  
  # stage1 air temp is Celsius
  arrow::open_dataset(s3) |>
    dplyr::filter(site_id == site,
                  variable %in% input_var) |>
    dplyr::arrange(datetime) |>
    dplyr::collect()
  
}


# Test this out:
future <- noaa_ensemble_forecast("BART",vars,"2022-06-01")

future_data <- vector(mode="list",length=length(testing_sites))
for (i in 1:length(testing_sites)) {
  print(i)
  future_data[[i]] <- noaa_ensemble_forecast(testing_sites[[i]],vars,"2022-06-01")
}


### Collate the data together, joining with the fluxes

forecast_data <- tibble(site = testing_sites,
                        training = historical_data,
                        testing = future_data) |>
  inner_join(summarized_fluxes,by="site") |>
  rename(fluxes = data) |>
  select(-n_obs) |>
  mutate(training = map2(.x=training,.y=fluxes,.f=~(.x |> pivot_wider(names_from = "variable",values_from = "prediction") |> inner_join(.y,by=c("datetime"="day")))))


save(forecast_data,file = 'data/forecast_data_ready.Rda')

forecast_data


# Make a plot of historical and forecast data at a site

driver_plot<- function(site_name,data_product,color_name) {
  
  training <- forecast_data |>
    filter(site == site_name,method=="dejong_shappert_1972") |>
    unnest(cols=c("training")) |>
    pivot_longer(cols=data_product) |>
    filter(name == data_product) |>
    mutate(value = if_else(name == "TSOIL",value-273.15,value))
  
  testing <- forecast_data |>
    filter(site == site_name,method=="dejong_shappert_1972") |>
    unnest(cols=c("testing")) |>
    select(-fluxes,-training) |>
    filter(variable == data_product) |>
    mutate(prediction = if_else(variable == "TSOIL",prediction-273.15,prediction)) |>
    group_by(datetime,variable) |>
    reframe(value =
              quantile(prediction,na.rm=TRUE,probs = c(0.025,0.5,.975),
              ),
            name = c("q0.025", "q0.5", "q0.975")) |> 
    pivot_wider()
  
  training |> ggplot(aes(x=datetime)) +
    geom_line(aes(y=value),color=color_name,size=1) +
    geom_line(data = testing,aes(y=q0.5),color=color_name,size=1) +
    geom_ribbon(data=testing,aes(ymin=q0.025,ymax=q0.975),alpha=0.3,fill=color_name) +  theme_minimal() +
    ggtitle(paste0("NEON Site: ",site_name) ) +
    xlab("Time")
    
    
  
}

driver_plot("UNDE","SOILW",'blue') + ylab(bquote(f[W]~'(no units)')) + ylim(c(0.1,0.4))

driver_plot("UNDE","TSOIL",'red') + ylab(bquote(T[S]~'(Celsius)'))

### Now we should be able to run a regression for each site on the training data
# Formula RS = kR*Q10^((Tsoil-273.15)/10)*h(fW) or
# ln(RS/h(fW)) = ln(kR) + ((Tsoil-273.15)/10) * Q10 or
# ln(flux2) = A + TSOIL2 * ln(Q10), where A = ln(kR), TSOIL2 = ((Tsoil-273.15)/10)
# and flux2 = RS/h(fW), where h(fW) = 3.11*SOIL2 - 2.42*SOILW^2

forecast_model <- function(training_data,testing_data) {
  
  train <- training_data |>
    mutate(flux2 = log(flux/(3.11*SOILW-2.42*SOILW^2)),
           TSOIL2 = (TSOIL-273.15)/10)
  # Plot
  #train |> ggplot(aes(x=datetime,y=flux)) + geom_line()
  # train |> ggplot(aes(x=TSOIL,y=flux)) + geom_point()
  fit_val <- lm(flux2 ~ TSOIL2,data = train) |> coefficients()
  
  kR <- exp(fit_val[1])
  Q10 <- exp(fit_val[2])
  
  
  # Now let's see how this does on our predictions:
  test <- testing_data |>
    group_by(ensemble) |>
    nest() |>
    mutate(predict = map(.x=data,.f=~(.x |> pivot_wider(names_from = "variable",values_from = "prediction") |>
                                        mutate(
                                          soilR_1 = kR*Q10^((TSOIL-273.15)/10)*(3.11*SOILW-2.42*SOILW^2),
                                          soilR_Le =  (0.598 + exp(0.044*(TSOIL-273.15)))
                                               ) |> 
                                        select(-TSOIL,-SOILW) |>
                                        pivot_longer(cols=c("soilR_1","soilR_Le"),names_to="variable",values_to="prediction") |>
                                        mutate(day = floor_date(datetime,unit="days")) |>
                                        group_by(day,variable) |>
                                        summarize(prediction = mean(prediction),.groups="drop"))
                         ))  |>
    select(-data) |>
    unnest(cols=c("predict")) |>
    ungroup()
  
  return(test)
  
}

forecast_model(forecast_data$training[[1]],forecast_data$testing[[1]])

out_forecasts <- forecast_data |>
  mutate(predict = map2(.x=training,.y=testing,.f=~forecast_model(.x,.y)))


## OK!  Let's score each one according to the CRPS
#https://projects.ecoforecast.org/neon4cast-docs/Evaluation.html

compute_crps <- function(forecasts,reference) {
  
  s <- 0
  for(i in 1:length(forecasts)){
    for(j in 1:length(forecasts)){
      s <- s + abs(forecasts[i] - forecasts[j])
    }
  }
  crps_equation_2 <- mean(abs(forecasts - reference)) - s / (2 * length(forecasts)^2)
  return(crps_equation_2)
  
}

### Test this out
compute_crps(rnorm(1000, mean = 8, sd = 1.0),8)

# We want to aggregate the ensembles up to the day




plot_index <- 30
out_forecasts$predict[[plot_index]] |>
  group_by(day,variable) |>
  reframe(value =
  quantile(prediction,na.rm=TRUE,probs = c(0.025,0.5,.975),
                                 ),
  name = c("q0.025", "q0.5", "q0.975")) |> 
  pivot_wider() |>
  #inner_join(out_forecasts$fluxes[[1]],by=c("datetime"="day"))
  mutate(variable = factor(variable,labels=c('soilR_1' = 'Empirical','soilR_Le' = 'Huong Le & Vargas (2024)'))) |>
  ggplot(aes(x=day)) + 
    geom_line(aes(y=q0.5,color=variable),size=1) +
    geom_ribbon(aes(ymin=q0.025,ymax=q0.975,fill=variable),alpha=0.3) +
  geom_point(data = summarized_fluxes$data[[plot_index]] ,aes(x=day,y=flux),color='purple',size=2) + 
  geom_line(data = summarized_fluxes$data[[plot_index]] ,aes(x=day,y=flux),color='purple',size=0.5) +
  labs(color = "Forecast model:",
       fill = "Forecast model:",
       y = bquote(~Flux~'('~mu~mol~m^-2~s^-1*~')'),
       x = "Time") + theme_minimal() +
  theme(legend.position='bottom') +
   ggtitle(paste0("NEON Site: ",out_forecasts$site[[plot_index]]) )

out_forecasts_small <- out_forecasts |>
  select(site,method,fluxes,predict) |>
  mutate(score_info = map2(.x=predict,.y=fluxes, .f=~(
    
    .x |> group_by(variable,day) |> 
      nest() |>
      inner_join(.y,by="day") |>
      mutate(score = map2_dbl(.x=data,.y=flux,.f=~compute_crps(.x$prediction,.y))) |>
      select(day,variable,score)
    
  )

                           )) |>
  select(site,method,score_info)
  

yee <- out_forecasts_small |>
  unnest(cols=c(score_info))

yee |>
  ggplot(aes(x=day,y=score,color=method)) + geom_line() +
  facet_wrap(.~site) + ylim(c(0,5))
# See a histogram of the scores
yee |>
  ggplot(aes(x=variable,y=score))+ geom_boxplot() + geom_jitter()
                                               
out_forecasts_small$fluxes[[1]] |> glimpse()
summarized_fluxes$data[[1]] |> glimpse()
forecast_data$fluxes[[1]] |> glimpse()

### Plot
out_forecasts$predict[[11]] |> 
  #filter(variable =="soilR") |>
  ggplot(aes(x = datetime, y = prediction, group = ensemble)) +
  geom_line() +
  facet_wrap(~variable, scales = "free")

# Plot:
future |> 
  ggplot(aes(x=datetime,y=prediction,group=ensemble)) + geom_line() +
  facet_grid(variable~.,scales = "free_y")


### Test this out
future2 <- noaa_mean_forecast(curr_site,var = vars,noaa_date)

# Plot:
future2 |> 
  ggplot(aes(x=datetime,y=prediction)) + geom_line() +
  facet_grid(variable~.,scales = "free_y")

#Step 3.0: Define the forecasts model for a site
forecast_site <- function(site) {
  message(paste0("Running site: ", site))
  
  # Get site information for elevation
  site_info <- site_data |> dplyr::filter(field_site_id == site)
  
 
  #  Get 30-day predicted temperature ensemble at the site
  noaa_future <- noaa_ensemble_forecast(site, c("TSOIL","SOILW"), noaa_date)
  
  # use an exponential function to forecast soil respiration each ensemble member
  forecast <- 
    noaa_future |> 
    group_by(ensemble) |>
    nest() |>
    mutate(predict = map(.x=data,.f=~(.x |> pivot_wider(names_from = "variable",values_from = "prediction") |>
                                        mutate(soilR = 0.03*3^((TSOIL-273.15)/10)) |> select(-TSOIL,-SOILW) |>
                                        pivot_longer(cols=c("soilR"),names_to="variable",values_to="prediction")))) |>
    select(-data) |>
    unnest(cols=c("predict")) |>
    ungroup()
  
  
  
  # # Format results to EFI standard
  # forecast <- forecast |>
  #   mutate(reference_datetime = forecast_date,
  #          family = "ensemble",
  #          model_id = model_id) |>
  #   rename(parameter = ensemble) |>
  #   select(model_id, datetime, reference_datetime,
  #          site_id, family, parameter, variable, prediction)
}

### AND HERE WE GO! We're ready to start forecasting ### 

## Test with a single site first!
forecast <- forecast_site( sites[1] )

#Visualize the ensemble predictions -- what do you think?
forecast |> 
  #filter(variable =="soilR") |>
  ggplot(aes(x = datetime, y = prediction, group = ensemble)) +
  geom_line() +
  facet_wrap(~variable, scales = "free")


# Run all sites -- may be slow!
forecast <- map_dfr(sites, forecast_site)

### NEXT STEP: GET PARAMETER VALUES FROM PECAN!!!!!! - # Just build an exponential model based on the one we have (it will be easier to work with, and time is short ....)




