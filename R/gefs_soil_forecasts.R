# Pull in forecasts for a given day of the year

#' @title gefs_soil_forecasts pulls in the forecasted covariates at NEON sites 
#'
#' @param input_date string in YYYY-MM-DD format
#' @param neon_site string (or list of strings of 4 digits neon sites)
#' @param env_vars vector of variables to collect from GEFS. Defaults to common soil values.
#' 
#' @import glue
#' @import arrow
#' @import dplyr
#' @import tidyr
#'
#' @returns tibble of data file with PRES, TSOIL, SOILW, WEASD, SNOD, ICETK and forecast at each NEON site.  See https://www.nco.ncep.noaa.gov/pmb/products/gfs/gfs.t00z.pgrb2.0p25.f003.shtml
#' @export
#'
#' @examples
#' 
#' curr_forecast <- gefs_soil_forecasts("2025-06-01")
gefs_soil_forecasts <- function(input_date,
                               neon_site,
                               env_vars = c("PRES","TSOIL", "SOILW", "WEASD", "SNOD","ICETK")
                               ) {
  
  bucket <- "bio230014-bucket01"
  path <- "neon4cast-drivers/noaa/gefs-v12/stage1"
  endpoint <- "https://sdsc.osn.xsede.org"
  
  stage1 <- 
    glue::glue("{bucket}/{path}/reference_datetime={input_date}") |>
    arrow::s3_bucket(endpoint_override = endpoint, anonymous = TRUE) |>
    arrow::open_dataset()
  
  
  # Grab the data we need
  forecast_data <- stage1 |> 
    dplyr::filter(variable %in% env_vars,
           site_id %in% neon_site) |> 
    dplyr::collect() |>
    dplyr::mutate(prediction = if_else(prediction ==9999.00,NA,prediction)) |>
    tidyr::pivot_wider(names_from = "variable",values_from = "prediction")
  
  return(forecast_data)
  
  
  
  
}
