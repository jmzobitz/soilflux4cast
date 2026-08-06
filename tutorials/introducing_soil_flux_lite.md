# Introduction to soil flux forecasting - lite (just the facts)
John Zobitz

# Theme: Soil fluxes

- **What:** Soil fluxes of carbon (gC m<sup>-2</sup> d<sup>-1</sup>)
  derived from Fick’s Law of diffusion.
- **Where:** 47 terrestrial NEON sites that span the diverse ecosystems
  of the U.S.
- **When:** Daily forecasts for at least 30-days in the future are
  accepted at any time. The only requirement is that submissions are
  predictions of the future at the time the forecast is submitted.
- **Why:** Soils are one of the largest pools of terrestrial carbon.

## Prerequisites

This module assumes you:

1.  Have experience working with creating and evaluating ecological
    forecasts.
2.  Understand what soil carbon fluxes are and how NEON measures data to
    compute soil carbon fluxes.
3.  Have experience working with `R` and `tidyverse`.

This tutorial utilizes the [`soilflux4cast` github
repository](https://github.com/jmzobitz/soilflux4cast), which I
anticipate developing further into a standalone `R` package.

### Libraries

To run this tutorial you will need the following R packages / libraries

- `tidyverse` (data wrangling)
- `jsonlite` (accessing github files)
- `devtools` (sourcing from a github repo)
- `glue` (file name pasting)
- `arrow` (file storage)
- `terra` (downloading forecasts)
- `sf` (downloading forecasts)

## About `soilflux4cast`

The [`soilflux4cast` github
repository](https://github.com/jmzobitz/soilflux4cast) provides the
following:

- **Drivers**: historical soil environmental driver data at each of the
  47 terrestrial NEON sites. These data are used for forecast
  parameterization. Historical driver data for the previous month are
  computed at the start of each month using a github action. Extends
  from 2022-01 to present. Stored as `csv` files.
- **Targets**: historical daily average soil fluxes at each of the
  terrestrial NEON sites. Computed with a github action on the 15th of
  each month, following a provisional NEON data release. Extends from
  2017-01 to present. Stored as `csv` files.

Run the following code to acquire the locally store functions that will
allow you to acquire driver and target data:

``` r
# Helper function to source from your specific repo
source_github <- function(file_name) {
  base_url <- "https://raw.githubusercontent.com/jmzobitz/soilflux4cast/main/"
  devtools::source_url(paste0(base_url, file_name))
}

# Now you can source any file by name
source_github("R/download_values.R")
source_github("R/noaa_soil_drivers.R")
```

## Drivers

Driver variables are provided by NOAA’s Global Ensemble Forecasting
System, acquired through the [`gefs4cast` github
repository](https://github.com/eco4cast/gefs4cast). Notably for this
project, we have pre-selected NOAA GEFS [driver
variables](https://www.nco.ncep.noaa.gov/pmb/products/gens/gec00.t00z.pgrb2a.0p50.f000.shtml)
that are known to influence soil surface fluxes:

- `PRES`: Surface Pressure \[Pa\]
- `TSOIL`: Soil Temperature 0-0.1 m below ground \[K\]
- `SOILW`: Volumetric Soil Moisture Content 0-0.1 m below ground
  \[Fraction\]
- `WEASD`: Water Equivalent of Accumulated Snow Depth \[kg/m^2\]
- `SNOD`: Snow Depth \[m\]
- `ICETK`: Ice Thickness \[m\]

If you would like additional variables included, please file a [github
issue](https://github.com/jmzobitz/soilflux4cast/issues).

### Historical (Stage 3) drivers

The function `download_values` is used to acquire across all terrestrial
NEON sites. For example to get the drivers from April 2025:

``` r
download_values(
  variable = "drivers",  # environmental variables
  year = "2025",
  month = "04"  # optional - you can omit if you want values for the entire year
  ) |> 
  dplyr::glimpse()
```

The output has the following variables:

- `site_id`: string : NEON site ID
- `datetime`: timestamp\[us, tz=UTC\]: datetime of forecast
- `PRES`: prediction of Surface Pressure \[Pa\]
- `TSOIL`: prediction of Soil Temperature 0-0.1 m below ground \[K\]
- `SOILW`: prediction of Volumetric Soil Moisture Content 0-0.1 m below
  ground \[Fraction\]
- `WEASD`: prediction of Water Equivalent of Accumulated Snow Depth
  \[kg/m^2\]
- `SNOD`: prediction of Snow Depth \[m\]
- `ICETK`: prediction of Ice Thickness \[m\]

### Ensemble forecasts (Stage 1)

The function `noaa_soil_drivers` is used to acquire ensemble GEFS
forecasts for driver variables at a given forecast date:

``` r
noaa_soil_drivers(
  forecast_date = "2025-05-01",
  # site = 'UNDE'   # optional to specify a given NEON site
  ) |>
  dplyr::glimpse()
```

The output has the following variables:

- `ensemble`: int32 : ensemble member number
- `cycle`: string: hour of day that forecast was started
- `horizon`: double : number of seconds in the future
- `datetime`: timestamp\[us, tz=UTC\]: datetime of forecast
- `family`: string: class of uncertainty (ensemble)
- `site_id`: string : NEON site ID
- `PRES`: prediction of Surface Pressure \[Pa\]
- `TSOIL`: prediction of Soil Temperature 0-0.1 m below ground \[K\]
- `SOILW`: prediction of Volumetric Soil Moisture Content 0-0.1 m below
  ground \[Fraction\]
- `WEASD`: prediction of Water Equivalent of Accumulated Snow Depth
  \[kg/m^2\]
- `SNOD`: prediction of Snow Depth \[m\]
- `ICETK`: prediction of Ice Thickness \[m\]

Other aboveground driver variables are available through the
[`neon4cast`
package](https://projects.ecoforecast.org/neon4cast-docs/Shared-Forecast-Drivers.html).

## Targets

The target is a daily total soil flux of carbon (gC m<sup>-2</sup>
d<sup>-1</sup>) derived from half-hourly computed soil carbon fluxes
with the [`neonSoilFlux` `R`
package](https://cran.r-project.org/web/packages/neonSoilFlux/index.html)
and associated publication
[LINK](https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210x.70216).

These targets are also accessible through the `download_values`
function:

``` r
download_values(
  variable = "targets",
  year = "2025",
  month = "04"  # optional - you can omit if you want values for the entire year
  ) |> 
  dplyr::glimpse()
```

The output has the following variables:

- `site_id`: string : NEON site ID
- `datetime`: timestamp\[us, tz=UTC\]: datetime of forecast
- `flux`: daily soil carbon flux \[gC/m2/d\]
- `flux_err`: daily soil carbon flux uncertainty \[gC/m2/d\]

Now you are ready to develop a model and forecast!

## Submitting forecasts

1.  Generate a forecast!
2.  Write the forecast output to a file that follows the [standardized
    format for the NEON EFI forecast
    challenge](https://projects.ecoforecast.org/neon4cast-ci/instructions.html#submission-process):
    `soil_flux-year-month-day-model_id.csv`. Compressed csv files with
    the csv.gz extension are also accepted. The year, month, and day are
    the year, month, and day the `reference_datetime` (`horizon = 0`).
3.  Submit your forecast to `zobitz@augsburg.edu` with the subject line
    soil_flux-year-month-day-model_id, along with an `R` function to run
    your forecast (if you want it to be automated)
4.  Register and describe your model here:
    [LINK](https://docs.google.com/forms/d/e/1FAIpQLScOnp6q5ODkPAIYplau-eWUE6mPEs4-9loMikOC6ugWgzGTkQ/viewform?usp=sharing&ouid=110173876465247112627).
    You are not required to register if your forecast submission uses
    the word “example” in your `model_id`. Any forecasts with “example”
    in the `model_id` will not be used in forecast evaluation analyses.
    You can use `neon4cast` as the challenge for which you are
    registering.
5.  Watch your forecast be evaluated as new data are collected.

## Other notes

- The `soilflux4cast` respository also computes daily averages of NEON
  soil temperature and soil water content when the data are available.
  These can be accessed an option with `download_values`:

``` r
download_values(
  variable = "soil-env",  # NEON measured data
  year = "2025",
  month = "04"  # optional - you can omit if you want values for the entire year
  ) |> 
  dplyr::glimpse()
```

This can be useful for driver comparison between NOAA and NEON.
