


#' targets_available is a helper function returns available soil fluxes on the soilflux4cast repo.  Uses httr JSONLite and stringR
#'
#' @param dates vector of dates to build target in YYYY-MM
#' 
#' @import httr
#' @import jsonlite
#' @import stringr
#' @import purrr
#' @import dplyr
#' @import tidyr
#'
#' @returns a tible of fluxes with their site_id, startDateTime, flux, and flux_err
#' @export
#'
#' @examples

targets_available <- function(dates=NULL) {
  
  # Build regex: match dates at the end before ".csv"
  pattern <- paste0("(", paste(dates, collapse = "|"), ")\\.csv$")
  
  # Define repo and path
  repo <- "jmzobitz/soilflux4cast"
  path <- "data/targets"
  
  # GitHub API URL
  url <- paste0("https://api.github.com/repos/", repo, "/contents/", path)
  
  # Get content
  res <- httr::GET(url)
  files <- jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"))
  
  # Base filter: keep only soil driver csv files with YYYY-MM
  files <- files |>
    dplyr::filter(stringr::str_detect(name,
                                      pattern = "(?<=neon_targets-)[:digit:]{4}-[:digit:]{2}.*\\.csv$"
    ))
  
  
  # If dates are provided, restrict further
  if (!is.null(dates)) {
    pattern <- paste0("(", paste(dates, collapse = "|"), ")\\.csv$")
    files <- files |>
      dplyr::filter(stringr::str_detect(download_url, pattern))
  }
  
  
  # Download and read the CSVs
  download_files <- files |>
    dplyr::mutate(data = purrr::map(
      .x = download_url,
      .f = ~ readr::read_csv(.x, show_col_types = FALSE)
    )) |>
    dplyr::select(data) |>
    tidyr::unnest(cols = c(data))
    
  return(download_files)
  
}
