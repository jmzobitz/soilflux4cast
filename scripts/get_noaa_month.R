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
  
  ### Example variable initialization
  # stage1 <- arrow::open_dataset(...)   # <--- YOUR SOURCE
  # env_vars <- c("tmp2m", "rh2m", ...)  # <--- YOURENV VARIABLES
  
  site_data <- readr::read_csv(
    "https://raw.githubusercontent.com/eco4cast/neon4cast-targets/main/NEON_Field_Site_Metadata_20220412.csv",
    show_col_types = FALSE
  ) %>%
    dplyr::filter(terrestrial == 1) %>%
    dplyr::select(field_site_id, field_latitude, field_longitude)
  
  driver_data <- stage1 %>%                            # <-- must already exist in your environment!
    dplyr::filter(
      variable %in% env_vars,
      site_id %in% site_data$field_site_id
    ) %>%
    dplyr::collect() %>%
    dplyr::mutate(prediction = dplyr::if_else(prediction == 9999.00, NA, prediction)) %>%
    tidyr::pivot_wider(names_from = variable, values_from = prediction) %>%
    dplyr::filter(ensemble == "gec00", horizon < 86400) %>%
    dplyr::mutate(datetime = lubridate::floor_date(datetime, unit = "day")) %>%
    dplyr::filter(datetime == forecast_date) %>%
    dplyr::group_by(site_id, datetime) %>%
    dplyr::summarize(
      dplyr::across(all_of(env_vars), ~ mean(.x, na.rm = TRUE)),
      .groups = "drop"
    )
  
  return(driver_data)
}

# ----- Run for each day -----
results <- purrr::map_df(all_dates, get_nooa_vars)

# ----- Write combined CSV -----
out_dir  <- "data/drivers"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

outfile <- file.path(out_dir, paste0("noaa_stage1_", month_input, ".csv"))
readr::write_csv(results, outfile)

message("Wrote NOAA monthly file: ", outfile)
