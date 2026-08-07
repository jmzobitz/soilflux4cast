### Plot of average soil flux in
library(tidyverse)
library(leaflet) # Making dynamic maps


# Now you can source any file by name
source("R/download_values.R")
source("R/noaa_soil_drivers.R")


## January 2026
jan_drivers <- download_values(
  variable = "targets",
  year = "2026",
  month = "01" # optional - you can omit if you want values for the entire year
) |>
  group_by(site_id) |>
  summarize(flux = mean(flux,na.rm=TRUE)) |>
  mutate(month = "January 2026",
         flux = pmax(flux,0)) |>
  ungroup()



## June 2026
jun_drivers <- download_values(
  variable = "targets",
  year = "2026",
  month = "06" # optional - you can omit if you want values for the entire year
) |>
  group_by(site_id) |>
  summarize(flux = mean(flux,na.rm=TRUE)) |>
  mutate(month = "June 2026",
         flux = pmax(flux,0)) |>
  ungroup()

drivers <- rbind(jan_drivers,jun_drivers)

# Make a simple map of NEON sites



site_data <- readr::read_csv(paste0("https://raw.githubusercontent.com/eco4cast/neon4cast-targets/","main/NEON_Field_Site_Metadata_20220412.csv")) |> 
  dplyr::filter(terrestrial == 1) |>
  inner_join(drivers,by=c("field_site_id"="site_id"))

### Load up data from 

# Create a color palette for the categorical variable
color_pal <- colorFactor(
  palette = "Set1", # Choose a palette from RColorBrewer or specify your own color:
  domain = str_to_title(site_data$phenocam_vegetation)
)

flux_pal <- colorNumeric(
  palette = "YlOrRd", # or "YlOrRd", "viridis"
  domain = c(0, 11)
)


site_data |> 
  filter(month == "January 2026") |> 
  leaflet() |> 
  addProviderTiles(providers$CartoDB.Positron) |> 
  addCircleMarkers(
    lng = ~field_longitude,
    lat = ~field_latitude,
    color = ~color_pal(str_to_title(phenocam_vegetation)), # Border color
    fillColor = ~flux_pal(flux),                           # Fill color (fixed argument name)
    radius = 10,
    weight = 3,                                            # Increased border weight for visibility
    fillOpacity = 0.6,
    opacity = 1
  ) |> 
  # Legend 1: Border (Vegetation Type)
  addLegend(
    position = "topright",
    pal = color_pal,
    values = ~str_to_title(phenocam_vegetation),
    title = "Vegetation Type",
    opacity = 1
  ) |> 
  # Legend 2: Fill (Flux Value)
  addLegend(
    position = "bottomleft",
    pal = flux_pal,
    values = ~flux,
    title = "Daily Average<br>Soil Flux<br>(gC m⁻² d⁻¹)",
    opacity = 1
  )


# Sample data
data.frame(
  lat = c(43.6532, 43.6426),
  lng = c(-79.3832, -79.3871)
) |>

leaflet() %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~lng, 
    lat = ~lat,
    # Border configurations
    stroke = TRUE,
    color = "#FF0000",       # Border color
    weight = 3,              # Border thickness
    # Fill configurations
    fill = FALSE,             # Must be TRUE
    fillColor = "#FF0000",   # Inside color
    fillOpacity = 0.6,       # Must be greater than 0 (0.0 to 1.0)
    radius = 8
  )
