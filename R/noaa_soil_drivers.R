
#' noaa_soil_drivers grabs the NOAA GEFS forecasts for soil variables at each terrestrial NEON site
#'
#' @param forecast_date Date of forecast (YYYY-MM-DD)
#' @param site (will be all terrestrial neon sites or a subset)
#'
#' @returns
#' @export
#'
#' @examples
#' noaa_soil_drivers("2025-06-01","UNDE")
noaa_soil_drivers <- function(forecast_date, site = NULL) {
  
  ### STEP 1: Acquire the NEON site_data 
  # Note: readr::read_csv is already namespaced correctly
  site_data <- readr::read_csv(
    paste0("https://raw.githubusercontent.com/eco4cast/neon4cast-targets/", "main/NEON_Field_Site_Metadata_20220412.csv"),
    show_col_types = FALSE
  ) |> 
    dplyr::filter(terrestrial == 1) |> 
    dplyr::select(field_site_id, field_latitude, field_longitude)
  
  if (!is.null(site)) {
    site_data <- site_data |>
      dplyr::filter(field_site_id %in% site)
  }
  
  bucket <- "bio230014-bucket01"
  path <- "neon4cast-drivers/noaa/gefs-v12/stage1"
  endpoint <- "https://sdsc.osn.xsede.org"
  
  # Namespacing glue and arrow
  stage1 <- 
    glue::glue("{bucket}/{path}/reference_datetime={forecast_date}") |>
    arrow::s3_bucket(endpoint_override = endpoint, anonymous = TRUE) |>
    arrow::open_dataset()
  
  env_vars <- c("PRES", "TSOIL", "SOILW", "WEASD", "SNOD", "ICETK")
  
  # Grab the data we need
  driver_data <- stage1 |> 
    dplyr::filter(variable %in% env_vars,
                  site_id %in% site_data$field_site_id) |> 
    dplyr::collect() |> # Added dplyr::
    dplyr::mutate(prediction = dplyr::if_else(prediction == 9999.00, NA_real_, prediction)) |> # Added dplyr::
    tidyr::pivot_wider(names_from = "variable", values_from = "prediction") # Added tidyr::
  
  # Remove any temporary files
  terra::tmpFiles(remove = TRUE)
  unlink(list.files(tempdir(), full.names = TRUE), recursive = TRUE)
  
  return(driver_data)
}