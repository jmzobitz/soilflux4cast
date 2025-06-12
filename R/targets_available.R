


#' targets_available is a helper function returns available dates (months) for target forecast on the soilflux4cast repo.  Uses httr JSONLite and stringR
#'
#' @import httr
#' @import jsonlite
#' @import stringr
#' @import purrr
#' @returns a string of dates
#' @export
#'
#' @examples
#' gef_dates()
targets_available <- function() {
  
  
  
  # Define repo and path
  repo <- "jmzobitz/soilflux4cast"
  path <- "data/targets"
  
  # GitHub API URL
  url <- paste0("https://api.github.com/repos/", repo, "/contents/", path)
  
  # Get content
  res <- httr::GET(url)
  files <- jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"))
  
  # View file names
  files$name |>
    stringr::str_subset(pattern="(?<=neon_targets-)[:digit:]{4}-[:digit:]{2}" ) |>
    purrr::discard(is.na)
  
  
  
  
}
