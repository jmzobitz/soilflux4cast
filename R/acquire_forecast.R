#' acquire_forecast Acquire the noon forecast cycle across all NEON sites at a given date.
#'
#' @param date specific day of the year in YYYY-MM-DD
#' @param forecast_vals the type of forecast. Can be c("gec00",sprintf("gep%02d", 1:30))
#' @param forecast_horizon how far out in the future will we run the forecast, can be f000, f024, f048, f072, f096, f120, f144, f168, f192, f216, f240, f264, f288, f312, f336, f360, f384
#' @param cycle which forecast cycle we want to do (00, 06, 12, 18)
#' @param variables which environmental variables we want to grab
#'
#' @returns A nested data table with forecast (gec00 - control or gepXX - ensemble), horizon (f000 - current or f024 - 24 hours out), depth (where measurement is valid, meters), TSOIL (Kelvin), SOILW - soil water as a percent
#' 
#' @export
#'
#' @examples
#' acquire_forecast("2024-01-01")
acquire_forecast <- function(date,
                             cycle = "00",
                             forecast_vals = c("gec00",sprintf("gep%02d", 1:30)),
                             forecast_horizon = c("f000", sprintf("f%03d", seq(24, 384, by = 24))),
                             variables = c("PRES","TSOIL", "SOILW", "WEASD", "SNOD","ICETK")
                             ) {
  
  
  # Acquire the NEON site_data 
  site_data <- readr::read_csv(paste0("https://raw.githubusercontent.com/eco4cast/neon4cast-targets/","main/NEON_Field_Site_Metadata_20220412.csv"),show_col_types = FALSE) |> 
    dplyr::filter(terrestrial == 1)|> 
    dplyr::select(field_site_id,field_latitude,field_longitude)
  
  
  # 1. Scrape the GEFS variable lookup table
  url <- "https://www.nco.ncep.noaa.gov/pmb/products/gens/gec00.t00z.pgrb2a.0p50.f000.shtml"
  page <- rvest::read_html(url)
  
  tables <- rvest::html_table(page, fill = TRUE)
  
  # Inspect to find the correct table (usually the first one here)
  lookup_table <- tables[[2]]
  
  # 6. Subset raster by desired variables (example here for the 4 variables)
  desired_vars <- variables
  selected_layers <- lookup_table |>
    dplyr::filter(Parameter %in% desired_vars)
  

  
  # This set of code defines the possible outcomes and forecast horizons. We iterate throgh the resulting loop
  
  forecast_values <- tidyr::expand_grid(forecast =  forecast_vals, 
                                 horizon = forecast_horizon) |>
    dplyr::mutate(
      gefs_object = purrr::map2_chr(
        .x = forecast,
        .y = horizon,
        .f = ~ paste0("gefs.", gsub("-", "", date), "/", cycle, "/atmos/pgrb2ap5/", .x, ".t", cycle, "z.pgrb2a.0p50.", .y)
      ), # Define the file name from AWS that we grab
      values = purrr::map(
        .x = gefs_object,
        .f = function(gefs_obj) {
          

          # Define the latitude and longitude of interest
          control_file_name <- paste0("temp_grib_file-",date)
          aws.s3::save_object(
            object = gefs_obj,
            bucket = "s3://noaa-gefs-pds/",
            file = control_file_name
          )
          
          # Open the GRIB2 file
          r <- terra::rast(control_file_name)
          
          
          
          # Identify the SOILW variable (replace with the actual variable name)
        
          soil_layers <- r[[selected_layers$Number]]
          names(soil_layers) <- selected_layers$Parameter
          # Extract value at the specified location - lots of if_else and a map
          
          # Extract value at the specified location - lots of if_else and a map
          soil_value <- site_data |>
            dplyr::mutate(soil_values = purrr::map2(.x = field_longitude, .y = field_latitude, , .f = ~ terra::extract(soil_layers, cbind(.x, .y)))) |>
            dplyr::mutate(forecast = purrr::map(soil_values, .f = ~ (.x |>
                                                                       tidyr::pivot_longer(cols = everything())
            ))) |>
            dplyr::select(field_site_id, forecast) |>
            tidyr::unnest(cols = c(forecast)) |>
            dplyr::mutate(cycle = cycle)
          
          
  
          
          # Remove the control file
          file.remove(control_file_name)
          
          return(soil_value)
        }
      )
    ) |>
    dplyr::select(-gefs_object) |>
    tidyr::unnest(cols = c(values))
 
  return(forecast_values)
   
}
  
