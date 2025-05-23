#' acquire_forecast Acquire the noon forecast cycle across all NEON sites at a given date.
#'
#' @param date specific day of the year in YYYY-MM-DD
#'
#' @returns A nested data table with forecast (gec00 - control or gepXX - ensemble), horizon (f000 - current or f024 - 24 hours out), depth (where measurement is valid, meters), TSOIL (Kelvin), SOILW - soil water as a percent
#' 
#' @export
#'
#' @examples
#' acquire_forecast("2024-01-01")
acquire_forecast <- function(date) {
  
  
  # Acquire the NEON site_data 
  site_data <- readr::read_csv(paste0("https://raw.githubusercontent.com/eco4cast/neon4cast-targets/","main/NEON_Field_Site_Metadata_20220412.csv"),show_col_types = FALSE) |> 
    dplyr::filter(terrestrial == 1)|> 
    dplyr::select(field_site_id,field_latitude,field_longitude)
  
  
  # This set of code defines the possible outcomes and forecast horizons. We iterate throgh the resulting loop
  
  forecast_values <- expand_grid(forecast =  c("gec00",sprintf("gep%02d", 1:30)), 
                                 horizon = c("f000","f024")) |>
    dplyr::mutate(
      gefs_object = map2_chr(
        .x = forecast,
        .y = horizon,
        .f = ~ paste0("gefs.", gsub("-", "", date), "/12/atmos/pgrb2ap5/", .x, ".t12z.pgrb2a.0p50.", .y)
      ), # Define the file name from AWS that we grab
      values = map(
        .x = gefs_object,
        .f = function(gefs_obj) {
          

          # Define the latitude and longitude of interest
          control_file_name <- "temp_grib_file"
          aws.s3::save_object(
            object = gefs_obj,
            bucket = "s3://noaa-gefs-pds/",
            file = control_file_name
          )
          
          # Open the GRIB2 file
          r <- terra::rast(control_file_name)
          
          
          
          # Identify the SOILW variable (replace with the actual variable name)
          soil_layers <- r[[grep("Soil Moisture|Soil Temperature", names(r), ignore.case = TRUE)]]
          
          # Extract value at the specified location - lots of if_else and a map
          soil_value <- site_data |>
            dplyr::mutate(soil_values = purrr::map2(.x = field_longitude, .y = field_latitude, , .f = ~ terra::extract(soil_layers, cbind(.x, .y)))) |>
            dplyr::mutate(forecast = purrr::map(soil_values, .f = ~ (.x |>
                                                                       tidyr::pivot_longer(cols = everything()) |>
                                                                       dplyr::mutate(depth = stringr::str_extract(name, pattern = "^.+(?=\\[m\\])")) |>
                                                                       dplyr::mutate(variable = stringr::str_extract(name, pattern = "(?<=;).+(?=\\[.+\\])")) |>
                                                                       dplyr::mutate(
                                                                         variable = dplyr::if_else(stringr::str_detect(name, pattern = "temperature"), "TSOIL", variable),
                                                                         variable = dplyr::if_else(stringr::str_detect(name, pattern = "Liquid Volumetric"), "L_SOILW", variable),
                                                                         variable = dplyr::if_else(stringr::str_detect(name, pattern = "(?<!Liquid[:space:])Volumetric"), "SOILW", variable)
                                                                       ) |>
                                                                       dplyr::select(-name) |>
                                                                       tidyr::pivot_wider(names_from = variable, values_from = value)
            ))) |>
            dplyr::select(field_site_id, forecast) |>
            tidyr::unnest(cols = c(forecast))
          
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
  
