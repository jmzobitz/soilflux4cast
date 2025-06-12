# Pull in forecasts for a given day of the year

# inputdate is in YYYY-MM-DD
#' Title gefs_forecast pulls in the acquired soil_forecast data 
#'
#' @param input_date string in YYYY-MM-DD format
#'
#' @returns tibble of data file with PRES, TSOIL, SOILW, WEASD, SNOD, ICETK and forecast at each NEON site.  See https://www.nco.ncep.noaa.gov/pmb/products/gfs/gfs.t00z.pgrb2.0p25.f003.shtml
#' @export
#'
#' @examples
#' 
#' curr_forecast <- geds_soil_forecast("2025-06-01")
gefs_soil_forecast <- function(input_date) {
  
  
  paste0("https://raw.githubusercontent.com/jmzobitz/soilflux4cast/refs/heads/main/data/drivers/forecast/forecast_soil_drivers-",input_date,".csv") |>
    readr::read_csv()
  
  
  
  
}
