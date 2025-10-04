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
    tidyr::pivot_wider(names_from = noaa,values_from = value) |>
    na.omit()
  
  target <- target_data |> 
    tidyr::pivot_wider(names_from = variable,values_from = observation) |>
    na.omit()
  
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
                  site_id %in% focal_sites,
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


