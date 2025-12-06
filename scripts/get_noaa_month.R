#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) stop("Usage: Rscript scripts/get_noaa_month.R YYYY-MM")

library(tidyverse)
library(lubridate)

month_input <- args[1]

# ----- Create all dates for the month -----
start_date <- as.Date(paste0(month_input, "-01"))
end_date   <- ceiling_date(start_date, "month") - days(1)
all_dates  <- seq.Date(start_date, end_date, by = "day")

# ----- Your namespaced function (inserted from earlier work) -----
get_nooa_vars <- function(forecast_date) {
  
  
  site_data <- readr::read_csv(
    "https://raw.githubusercontent.com/eco4cast/neon4cast-targets/main/NEON_Field_Site_Metadata_20220412.csv",
    show_col_types = FALSE
  ) |>
    dplyr::filter(terrestrial == 1) |>
    dplyr::select(field_site_id, field_latitude, field_longitude)
  
  bucket <- "bio230014-bucket01"
  path <- "neon4cast-drivers/noaa/gefs-v12/stage1"
  endpoint <- "https://sdsc.osn.xsede.org"
  
  stage1 <- 
    glue::glue("{bucket}/{path}/reference_datetime={forecast_date}") |>
    arrow::s3_bucket(endpoint_override = endpoint, anonymous = TRUE) |>
    arrow::open_dataset()
  
  env_vars <- c("PRES","TSOIL", "SOILW", "WEASD", "SNOD","ICETK")
  
  driver_data <- stage1 |>   
    dplyr::filter(
      variable %in% env_vars,
      site_id %in% site_data$field_site_id
    ) |>
    dplyr::collect() |>
    dplyr::mutate(prediction = dplyr::if_else(prediction == 9999.00, NA, prediction)) |>
    tidyr::pivot_wider(names_from = variable, values_from = prediction) |>
    dplyr::filter(ensemble == "gec00", horizon < 86400) |>
    dplyr::mutate(datetime = lubridate::floor_date(datetime, unit = "day")) |>
    dplyr::group_by(site_id, datetime) |>
    dplyr::summarize(
      dplyr::across(all_of(env_vars), ~ mean(.x, na.rm = TRUE)),
      .groups = "drop"
    )
  
  return(driver_data)
}

# ----- Run for each day -----
results <- purrr::map_df(all_dates, get_nooa_vars)

# ----- Write combined CSV -----
out_dir  <- "data/drivers/noaa"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

outfile <- file.path(out_dir, paste0("noaa_stage1_", month_input, ".csv"))
readr::write_csv(results, outfile)

message("Wrote NOAA monthly file: ", outfile)
