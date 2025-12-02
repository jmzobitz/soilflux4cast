# Heler script to compute out 

library(tidyverse)
source('R/gefs_soil_forecasts.R')


  input_neon <- paste0('data/drivers/neon_soil_drivers-',curr_month,'.csv')
  
  neon_data <- read_csv(input_neon)
  
  # Identify neon_sites
  neon_sites <- neon_data$site_id |> unique()
  
  # Convert to first and last day of month
  start_date <- ymd(paste0(curr_month, "-01"))
  end_date <- ceiling_date(start_date, unit = "month") - days(1)
  
  # Create the sequence
  my_dates <- seq.Date(start_date, end_date, by = "day") |>
    as.character()
  
  out_values <- vector(mode = "list",length=length(neon_sites))
  
  
  
  for (i in seq_along(out_values)) {
    curr_site <- neon_sites[[i]]   # Will be a loop here
    
    neon_subset <- neon_data |>
      filter(site_id == curr_site)
    
    forecast_vals <- tibble(date = my_dates,
                            forecast = map(.x=date, .f=~(gefs_soil_forecasts(.x,neon_site = curr_site,env_vars = c("TSOIL","SOILW")) |>
                                                           filter(ensemble == "gec00",horizon == 0)),.progress = TRUE))
    
    # Then once that is fixed, we will filter and join to the neon_data
    gefs_site <- forecast_vals |>
      unnest(cols = c(forecast)) |>
      select(date,TSOIL,SOILW) |>
      # mutate(type = "gefs") |>
      mutate(TSOIL = TSOIL - 273.15) |>   # Convert to Celsius
      rename(startDateTime=date,
             VSWC = SOILW) |>
      pivot_longer(cols=c(TSOIL,VSWC)) |>
      rename(gefs = value)
    
    
    neon_gefs <- neon_subset |>
      mutate(startDateTime  = as.character(startDateTime)) |> 
      rename(TSOIL = SOILT) |>
      pivot_longer(cols=c(TSOIL,VSWC)) |>
      rename(neon=value) |>
      left_join(gefs_site,by=c("startDateTime","name")) |>
      group_by(site_id,name) |>
      nest()
    
    out_values[[i]] <- neon_gefs |>
      mutate(taylor = map(.x=data,.f=~(.x |>
                                         drop_na() |>
                                         summarize(sd_meas=1,
                                                   sd=sd(neon)/sd(gefs),
                                                   r=cor(gefs,neon),
                                                   centered_rms=sd((gefs-mean(gefs))-((neon-mean(neon))))/sd(gefs)
                                         ) |>
                                         mutate(x_coord = sd*r, y_coord = sd*sin(acos(r)))))) |>
      select(-data) |>
      unnest(cols=c(taylor)) |>
      mutate(month = curr_month) |>
      relocate(site_id,month)
    
    
    
  }
  
  # Now we want to save the values for the given month
  
  taylor_out <- bind_rows(out_values)
  
  write_csv(taylor_out,file = paste0('data/validation/noaa-gefs-validate-',curr_month,'.csv'))
