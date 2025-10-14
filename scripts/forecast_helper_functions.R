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
    dplyr::collect()
  
  
}

# Make a prediction based on a model
make_prediction <- function(noaa_future,
                            fit_model,
                            target_variable) {
  
  future_data <- noaa_future |>
    tidyr::pivot_wider(names_from = variable,values_from = prediction)
  
  future_data |> 
    dplyr::mutate(prediction = predict(fit_model, future_data),
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