### Scripts for useful forecasting information
### Author: JMZ
### Modified 10/3/25


# the function plot targets
plot_targets <- function(target_data,
                         measurement,
                         site,
                         year) {
  
  target_data |>
    dplyr::filter(site_id == site,
                  lubridate::year(datetime) %in% year,
                  variable == measurement)  |>
    dplyr::mutate(datetime = as.Date(datetime)) |>
    ggplot2::ggplot(aes(x = datetime, y = observation)) +
    ggplot2::geom_point() +
    ggplot2::ylab(measurement) +
    ggplot2::scale_x_date(date_labels = "%Y-%m-%d") +
    ggplot2::ggtitle(site)
  
  
}

# Download mean historical data (for building forecasts)
noaa_mean_historical <- function(site,
                                 year) {
  
  year_start <- ymd(paste0(year,'-01-01'))
  year_end <- ymd(paste0(year,'-12-31'))
  
  neon4cast::noaa_stage3() |>
    dplyr::filter(site_id %in% site,
                  datetime >= year_start,
                  datetime <= year_end,
                  #variable %in% met_variable
    )|> 
    dplyr::rename(ensemble = parameter) |>
    dplyr::select(site_id,datetime, variable,prediction, ensemble) |>
    dplyr::mutate(date = lubridate::as_date(datetime)) |>
    dplyr::group_by(site_id,date,variable) |>
    dplyr::summarize(value = mean(prediction, na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::rename(datetime = date,
                  noaa = variable) |>
    dplyr::collect()
}


# the function plot noaa
plot_noaa <- function(noaa_data,
                      measurement,
                      site) {
  
  noaa_data |>
    dplyr::filter(site_id == site,
                  noaa == measurement)  |>
    ggplot2::ggplot(aes(x = datetime, y = value)) +
    ggplot2::geom_point() +
    ggplot2::ylab(measurement) +
    ggplot2::scale_x_date(date_labels = "%Y-%m-%d") +
    ggplot2::ggtitle(site)
  
  
}

# Plot together a variable with the forecast target
plot_target_noaa <- function(target_data,
                             noaa_data,
                             noaa_variable,
                             target_variable) {
  
  noaa <- noaa_data |> dplyr::filter(noaa == noaa_variable) |> na.omit()
  target <- target_data |> dplyr::filter(variable == target_variable) |> na.omit()
  
  noaa |>
    dplyr::inner_join(target,by=c("site_id","datetime")) |>
    ggplot2::ggplot(aes(x=value,y=observation)) + 
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method='lm',se=FALSE) +
    ggplot2::xlab(noaa_variable) +
    ggplot2::ylab(target_variable)
  
}

# Determine a regression equation for a model
fit_lm_model <- function(target_data,
                         noaa_data,
                         regression_equation) {
  
  noaa <- noaa_data |>
    tidyr::pivot_wider(names_from = noaa,values_from = value) 
  
  target <- target_data |> 
    tidyr::pivot_wider(names_from = variable,values_from = observation) 
  
  data_together <- noaa |>
    dplyr::inner_join(target,by=c("site_id","datetime"))
  
  fit_model <- lm(regression_equation,data=data_together)
  
  return(fit_model)
  
  
}


# Now forecast on the site
get_forecast_noaa <- function(site,
                              forecast_date,
                              met_variables) {
  
  forecast_date <- as.Date(forecast_date) 
  noaa_date <- forecast_date - lubridate::days(1)
  
  weather_future_s3 <- neon4cast::noaa_stage2(start_date = as.character(noaa_date))
  
  weather_future <- weather_future_s3 |> 
    dplyr::filter(datetime >= forecast_date,
                  site_id %in% site,
                  variable %in% met_variables) |> 
    dplyr::collect()  |>
    tidyr::pivot_wider(names_from = variable,values_from = prediction)
  
  
}

# Make a prediction based on a model
make_prediction <- function(noaa_future,
                            fit_model,
                            target_variable) {
  
  
  future_data |> 
    dplyr::mutate(prediction = predict(fit_model, noaa_future),
                  variable = target_variable) |>
    dplyr::select(parameter,datetime,family,site_id,reference_datetime,prediction,variable)
  
}


# Functions to help in model assessment, adapted from
# https://github.com/OlssonF/Forecast-evaluation-EFI25/blob/main/tutorial/forecast-evaluation-tutorial.md


# This function will compute the relevant stats for a horizon for a given forecast-observation pair
compute_forecast_stats <- function(forecast,
                                   targets) {
  
  curr_horizon <- targets |>
    dplyr::pull(duration) |>
    unique() |>
    {\(x) ifelse(x == "P1D", "day", "week")}()
  
  
  forecast_start <- forecast |> 
    dplyr::pull(datetime) |>
    min() |>
    lubridate::floor_date(unit="week",week_start = 1)
  
  forecast_end <- forecast |> 
    dplyr::pull(datetime) |>
    max() 
  
  # Create intervals so we can easily figure out where each forecast goes
  interval_starts <- seq(from = forecast_start, to = forecast_end, by = curr_horizon)
  
  if( curr_horizon == "week") {
    interval_ends <- interval_starts + lubridate::weeks(2)
  } else {
    interval_ends <- interval_starts + lubridate::days(1)
  }
  
  interval_length <- lubridate::interval(interval_starts[interval_ends <= forecast_end], interval_ends[interval_ends <= forecast_end])
  
  forecast_ref_time <- forecast$reference_datetime[[1]]  # When the forecast was made
  
  targets_adj <- targets |>
    dplyr::filter(between(datetime,forecast_start,forecast_end)) |>
    dplyr::mutate(interval_id = sapply(datetime, function(d)
      which(lubridate::`%within%`(d, interval_length))[1])) |>
    tidyr::drop_na()
  
  forecast |>
    dplyr::mutate(interval_id = sapply(datetime, function(d)
      which(lubridate::`%within%`(d, interval_length))[1])) |>
    tidyr::drop_na() |>
    dplyr::group_by(site_id,variable,interval_id) |>
    dplyr::reframe(value =
                     stats::quantile(prediction,na.rm=TRUE,probs = c(0.025,0.10,0.5,0.9,.975),
                     ),
                   name = c("q0.025", "q0.10","q0.5", "q0.90","q0.975"),
                   mean = mean(prediction, na.rm = TRUE),
                   sd = sd(prediction, na.rm = TRUE)
    ) |> 
    tidyr::pivot_wider() |>
    dplyr::inner_join(drop_na(targets_adj),by=c("site_id","interval_id","variable")) |>
    dplyr::mutate(crps = purrr::map2_dbl(.x=mean,.y=observation,.f=~{
      s <- 0
      for (i in seq_along(.x)) {
        for (j in seq_along(.x)) {
          s <- s + abs(.x[i] - .x[j])
        }
      }
      mean(abs(.x - .y)) - s / (2 * length(.x)^2)
    }),
    reference_datetime = lubridate::as_date(forecast_ref_time) ) |>
    dplyr::relocate(datetime,observation,reference_datetime,.after = site_id) |>
    dplyr::select(-project_id,-duration,-interval_id) |>
    dplyr::ungroup()
  
  
  
}

# Find the percentage of measurements within the confidence intervals
compute_reliability <- function(single_forecast) {
  
  single_forecast |> 
    dplyr::mutate(within_PI = dplyr::between(observation, q0.025, q0.975)) |> 
    dplyr::group_by(site_id) |> 
    dplyr::summarise(within_TRUE = mean(within_PI)*100,
                     within_FALSE = mean(!within_PI)*100,
                     
                     .groups = 'drop')
  
}

## EnFK adapted from 
# https://github.com/MacrosystemsEDDIE/module7_R/blob/main/assignment/R/EnKF_and_plot_functions.R

EnKF <- function(forecast, 
                 new_observation, 
                 ic_sd){
  
  #Allocate matrices - everything has to be converted to 
  #a matrix to do the matrix math required for the ensemble Kalman filter
  x_corr <- matrix(forecast)
  y <- matrix(new_observation)
  h_matrix <- matrix(0, nrow = 1, ncol = 1)
  R_matrix <- matrix(0, nrow = 1, ncol = 1)
  dit <- matrix(NA, nrow = length(x_corr[,1]), ncol = 1) 
  y_corr <- matrix(NA, nrow =  length(x_corr[,1]), ncol = length(y))
  x_update <- matrix(NA, nrow = length(x_corr[,1]), ncol = 1)
  
  #Only do EnKF if observations are present that day
  #there has to be at least 1 non-NA observation.
  if(length(which(!is.na(y))) > 0){
    
    #Assign observations to depths
    h_matrix[1, 1] <- 1
    
    #Create observational uncertainty matrix
    R_matrix[1,1] <- ic_sd^2
    
    #Calculate mean prediction for each depth
    ens_mean <- colMeans(x_corr)
    
    #Loop through ensemble members
    for(m in 1:length(x_corr[,1])){  
      #Ensemble specific deviation
      dit[m, ] <- x_corr[m, ] - ens_mean
      
      #if the first ensemble then create the matrix that is then averaged
      if(m == 1){
        p_it <- dit[m, ] %*% t(dit[m, ]) 
      }else{
        #if not the first ensemble then add the matrix to the previous matrix
        p_it <- dit[m, ] %*% t(dit[m, ]) +  p_it 
      }
    }
    
    #Calculate Cxx matrix
    Cxx_matrix <- p_it / (length(x_corr[,1]) - 1)
    
    #Add noise to observations
    for(m in 1:length(x_corr[,1])){
      y_corr[m, ] <- y + t(rmvnorm(n = 1, mean = c(0), sigma = R_matrix))
    }
    
    #Calculate Kalman Gain
    K <- Cxx_matrix %*% t(h_matrix) %*% solve(h_matrix %*% Cxx_matrix %*% t(h_matrix) + R_matrix)
    
    #Update model states based on Kalman Gain and devivations
    for(m in 1:length(x_corr[,1])){
      x_update[m, ] <- x_corr[m,] + K %*% (y_corr[m,] - h_matrix %*% x_corr[m,])
    }
  }else{
    #Only add noise if observations are missing
    x_update <- x_corr
  }
  
  ic_update <- c(x_update[,1])
  return(ic_update)
}


### Applies the ENKF with a given standard deviation

iterate_forecast <- function(forecast,
                             targets,
                             ic_sd) {
  
  curr_horizon <- targets |>
    dplyr::pull(duration) |>
    unique() |>
    {\(x) ifelse(x == "P1D", "day", "week")}()
  
  
  forecast_start <- forecast |> 
    dplyr::pull(datetime) |>
    min() |>
    lubridate::floor_date(unit="week",week_start = 1)
  
  forecast_end <- forecast |> 
    dplyr::pull(datetime) |>
    max() 
  
  # Create intervals so we can easily figure out where each forecast goes
  interval_starts <- seq(from = forecast_start, to = forecast_end, by = curr_horizon)
  
  if( curr_horizon == "week") {
    interval_ends <- interval_starts + lubridate::weeks(2)
  } else {
    interval_ends <- interval_starts + lubridate::days(1)
  }
  
  interval_length <- lubridate::interval(interval_starts[interval_ends <= forecast_end], interval_ends[interval_ends <= forecast_end])
  
  forecast_ref_time <- forecast$reference_datetime[[1]]  # When the forecast was made
  
  targets_adj <- targets |>
    dplyr::filter(between(datetime,forecast_start,forecast_end)) |>
    dplyr::mutate(interval_id = sapply(datetime, function(d)
      which(lubridate::`%within%`(d, interval_length))[1])) |>
    tidyr::drop_na() |>
    dplyr::group_by(site_id,variable,interval_id) |>
    nest()
  
  forecasts_nest <- forecast |>
    dplyr::mutate(interval_id = sapply(datetime, function(d)
      which(lubridate::`%within%`(d, interval_length))[1])) |>
    tidyr::drop_na() |>
    dplyr::group_by(site_id,variable,interval_id) |>
    nest()
  
  
  forecasts_nest |>
    left_join(targets_adj,by=c("site_id","variable","interval_id")) |>
    mutate(new_data = map2(.x=data.x,.y=data.y,.f=function(x,y) {
      
      if(is.null(y)) {
        return(x)
      } else {
        
        old_forecast <- x$prediction
        new_obs <- y$observation
        x$prediction <- EnKF(old_forecast,new_obs,ic_sd) 
        
        return(x)
        
      }
      
    })) |>
    select(-data.x,-data.y,-interval_id) |>
    unnest(cols=c(new_data)) |>
    ungroup()
  
  
  
  
  
}



