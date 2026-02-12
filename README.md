# soilflux4cast


This repository contains information to run a soil flux forecast at terrestrial NEON sites.

**Where to begin**? The best place to see what this repository can do is with the tutorial [Introduction to soil flux forecasting](https://github.com/jmzobitz/soilflux4cast/blob/main/tutorials/introducing_soil_flux.md)

Other helpful information:
- `data/drivers` contains the monthly files with daily averaged driver variables observed at NEON sites (soil water content and soil temperature) and from NOAA GEFS. This is useful for forecast development and testing.
- `data/outputs` contains modeled output for daily soil flux forecasts (automated through a github action `Daily Prediction Update`)
- `data/targets` contains the computed daily soil flux (gC m^-2^ d^-1^) at terrestrial NEON sites; the data latency is 1 month.
- `R/` contains useful `R` functions for analysis
- `scripts/` are useful scripts files used to build and evaluate forecasts.
  - `gefs_soil_forecasts` acquires covariates at a select NEON site.
  - `tutorials` are short vignettes on developing [soil flux forecasts](https://github.com/jmzobitz/soilflux4cast/blob/main/tutorials/introducing_soil_flux.md) and about [soil fluxes](https://github.com/jmzobitz/soilflux4cast/blob/main/tutorials/module-v2.md) in general.
  

## Technical notes
[Technical notes](documentation/00-technical-notes.md)

THE FOLLOWING BELOW IS TEXT TO BE EDITED FROM THE NEON4CAST EXAMPLE TEMPLATE

## Applying this repository to a new forecast

1) Click "Use This Template" to copy this example to your Github account.
1) Modify `forecast_model.R` to make your forecast model .  Many of the components you need to generate the forecast, including downloading NOAA weather forecasts, downloading target data, generating forecast files, generating metadata, validating files, and submitting forecasts. Avoid running the `neon4cast::submit()` function at the end of `forecast_model.R` until you are ready to submit a forecast to the Challenge.  It is important that you do NOT change the name of the file.  GitHub Actions (below) is looking for this file name. Be sure to change your `model_id`
2) Commit and push the changes to `forecast_model.R` to Github. 

## Manually running forecast in GitHub actions

1) Under the actions tab, click on ".github/workflows/do_prediction.yml" on the left side.
2) Click "Run workflow", then the green "Run workflow" button. 

## Automatically running forecast in GitHub actins

The forecast in this repository is designed to run daily at 20:00 UTC.  The execution of the forecast occurs on GitHub's servers, so your local computer does not need to be turned on.  In ".github/workflow/do_prediction.yml", the lines `-cron: "* 20 * *"` define the time that the forecast is run.  In this case it is run each day at 20:00:00 UTC (note all GitHub timings are on UTC).  You can update this to run on a different schedule based on timing codes found in https://crontab.guru

To start the automated forecast generation
1) Find the file `do_predictions.yml` in the `.github/workflows` on GitHub.
2) Click the edit (pencil) button to edit the file.
3) Remove the `#` before the words "schedule" and "-cron".  See below:

Change

```
on:
  workflow_dispatch:
  #schedule:
  #- cron: "0 20 * * *"
```
to
```
on:
  workflow_dispatch:
  schedule:
  - cron: "0 20 * * *"
```

A video describing how to use GitHub actions for automated forecast generation can be found here: https://youtu.be/dMrUlXi4_Bo

## Running in mybinder


