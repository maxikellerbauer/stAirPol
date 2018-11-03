# REAMDE

## German
  - st_modelling_of_air_pollution_kellerbauer.pdf
  Digitale Version der Masterarbeit
  - data/
  Minimale Datensammlung, in der alle Daten für die Münchener Innenstadt für den Monat Dezember 2017 vorhanden sind. Die Daten sind ausreichend, um für die Münchener Innenstadt für den Monat Dezember 2017 beliebige Modellierungen vorzunehmen.
  - stAirPol/
  Ein für die Masterarbeit entwickeltes R-Package, in dem alle Funktionalitäten gesammelt wurden, um die raumzeitliche Modellierung vorzunehmen. Mit Hilfe dieses R-Packages ist es möglich, alle in der Arbeit vorgestellten Ergebnisse zu reproduzieren.
    - man/
    - R/
    - src/
    - data/
    Mehrere kleine Datensätze, welche innerhalb des R-Packages geladen werden können. Diese werden benötigt, um die Vignetten 3 bis 6 auszuführen.
    - vignettes/
    In diesem Ordner sind sechs R-Skripte enthalten. Die ersten beiden behandeln den Download der Open Data und die Zusammenfassung in weiterverarbeitbare Dateien. Die anderen vier zeigen verschiedene Arten die Modellierung auf, und stellen die meisten in diesem R-Package implementierten Funktionen vor.
      - 1_data_download.R
      R-Script, zum Download aller für die Modellierung notwendigen Daten aus den Open Data Quellen. Es werden stets alle Daten, um eine beliebige Modellierung in Deutschland durchzuführen, in dem ausgewählten Zeitraum heruntergeladen. Dies entspricht jeden Monat ungefähr 20 bis 30 Gigabyte, abhängig davon, welcher Monat gewählt wird. Der Downloadprozess kann pro Monat von einigen Stunden bis zu einem Tag dauern.
      - 2_data_preparation.R
      R-Script, zur Aufarbeitung der in 1_data_download.R geladenen Daten. Hier werden anhand es selbstgewählten Zeitraumes Aggregationen der Daten erstellt. Dabei werden vier verschieden Datensätze generiert. Jeweils für jede Partikelgrö"se ein Datensatz, welcher für die Modellierung genutzt wird, und ein Datensatz, welcher ein reguläres Gitter mit allen nötigen Kovariablen bereitstellt, um eine räumliche Vorhersage auf dem Gitter zu ermöglichen.
      - 3_simple_modelling_and_crossvalidation.R
      R-Script, welches einfache Modellierungen der Daten zeigt.
      - 4_advanced_modelling.R
      R-Script, welches weitere Modellierungen der Daten zeigt.
      - 5_temporal_predictions.R
      R-Script, in dem zeitliche Vorhersagen, wie sie in Kapitel \ref{temporal vorgestellt wurden, durchgeführt werden.
      - 6_aggregation_to_administrative_level.R
      R-Script, welches eine Aggregation auf administrativer Ebene, wie in Kapitel \ref{mucmaps vorgestellt, durchführt.


## Englisch
- st_modelling_of_air_pollution_kellerbauer.pdf
Digital version of the master thesis
- date/
Minimal data collection, all data for the Munich city center for December December 2017 are available. The data is sufficient for modelling Munich for the month of December 2017.
- stAirPol /
An R-Package developed for this Master's thesis, in which all functionalities have been considered in order to perform the spatiotemporal modeling. With the help of this R-package it is possible to reproduce all results of the master thesis.
- man/
- R /
- src /
- data/
Several small data sets that can be loaded within the R-Package. These are needed to run the vignettes 3 to 6.
- vignettes /
This folder contains six R scripts. The first two deal with the download of the open data. The other four show different types of modeling, and most of the features implemented in this R package.
- 1_data_download.R
R-Script, to download all data required for modeling from the Open Data sources. This data is about 20 to 30 gigabytes each month, depending on which month is chosen. The download process can take from a few hours to a day per month.
- 2_data_preparation.R
R script, for processing the data loaded in 1_data_download.R. Here, based on the self-selected time period, aggregations of the data are created. This generates four different data sets. For each PM, which is used for modeling, a data set is provided.
- 3_simple_modelling_and_crossvalidation.R
R-Script, which shows simple modelling of the data.
- 4_advanced_modelling.R
R-Script, which shows advanced modelling of the data.
- 5_priority_predictions.R
R-Script, in which temporal forecasts, is applied
- 6_aggregation_to_administrative_level.R
R-Script, which performs an aggregation at the administrative level.
