#' download_annual_values Acquire the value across all NEON sites at a given date.
#'
#' @param variable do we get targets or drivers?
#' @param year year (as a string)
#' @param month month (as a string, 01 02 03 ...)
#' 
#' @returns A data table of outputs
#' 
#' @export
#'
#' @examples
#' download_annual_values("drivers","2024")

download_annual_values <- function(variable = "drivers", year, month = NULL) {
  
  # Ensure character types
  year  <- as.character(year)
  month <- if (!is.null(month)) sprintf("%02d", as.integer(month)) else NULL
  
  # GitHub API endpoint for directory contents
  api_url <- paste0(
    "https://api.github.com/repos/jmzobitz/soilflux4cast/contents/data/",
    variable
  )
  
  # Get file listings
  files <- jsonlite::fromJSON(api_url)
  
  # Build matching pattern
  if (is.null(month)) {
    # Match an entire year: e.g., "2017-"
    grep_string <- paste0(year, "-.*\\.csv$")
  } else {
    # Match specific year + month: e.g., "2017-01"
    grep_string <- paste0(year, "-", month, ".*\\.csv$")
  }
  
  # Filter matching files
  variables <- dplyr::filter(files, grepl(grep_string, name))
  
  # Download & combine into a nested tibble
  variables_out <- tibble::tibble(
    url  = variables$download_url,
    data = purrr::map(url, ~ readr::read_csv(.x, show_col_types = FALSE))
  ) |>
    dplyr::select(data) |>
    tidyr::unnest(cols = c(data))
  
  return(variables_out)
}
