# REAMDE

## General Notes

This package was built during my master thesis (Spatio-temporal modelling of air pollution open data). With the help of this R-package it is possible to reproduce all results of the master thesis.

It's possible to use that package for a spatio-temporal modelling of air pollution (particulate matter:  $\text{PM}_10$, $\text{PM}_{2.5}$) in subareas of Germany. With some adjustments, it should be possible to model areas outside of Germany, or modelling some other air pollution substances like $O_3$ or $NO_x$.

This package takes the air pollution data from www.luftdaten.info, combines that data with metrological data from the Deutschen Wetter Dienst and aggregates it to different time intervals. With that aggregated data it's possible to perform Bayesian hierarchical spatio temporal modelling on it. The Bayesian Modelling is based on the spTimer package.

For installation use:

```
devtools::install_github('maxikellerbauer/stAirPol')
```

## Vignettes

This package contains six vignettes. These six vignettes are grouped in two different tasks.

(1) Data
The first two vignettes deal with the download of the open data and the aggregation of it.
- 1_data_download.R
R-Script, to download all data which is required for modelling from the Open Data sources. This data is about 20 to 30 gigabytes per month, depending on which month is chosen. The download process can take from a few hours to a day per month- 2_data_preparation.R
R script, for processing the data downloaded in 1_data_download.R. Here, based on the self-selected time period, aggregations of the data are created. This generates four different data sets. For each PM, which is used for modelling, a data set is provided.

(2) Modelling
- 3_simple_modelling_and_crossvalidation.R
R-Script, which shows a very simple spatio-temporal modelling of the data.
- 4_advanced_modelling.R
R-Script, which shows advanced spatio-temporal modelling of the data.
- 5_priority_predictions.R
R-Script, in which temporal forecasts, are applied
- 6_aggregation_to_administrative_level.R
R-Script, which performs an aggregation at the administrative level.
