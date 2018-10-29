require(data.table)
require(stAirPol)
require(ggplot2)



# User Input --------------------------------------------------------------
#' Spezify the data path which contains the collected data.
path = '~/stAirPol_data'

#' For which German Postcode do you want to model air pollution data?
#' Use directly one Postcode or see ?get_postcodes_for_landkreis and
#' ?get_postcodes_for_bundesland
#' We will take Munich for that example
m.plz <- c(80539)

#' Now you have to decide what the gridcellsize should be used, see
#' ?sf::st_make_grid for informations about the cellsize parameter
m.grid_cellsize <- 0.004

#' The next spezification which is needed is the timerange, please specify
#' the start and the end date
#' NOTE: currently are only whole months supported, so the startdate is floored
#' and the end_date is ceiilinged.
start_date = "2017-12-01"
end_date = "2017-12-05"

#' The spezification which is needed is the aggreation interval and the
#' timeshift which is apply to the data. We choose an aggregation_interval of 8
#' hours, for more information about the aggregration_interval units see
#' ?lubridate::round_date. The timeshift is applied to the data.
#' Run the print method on the object for more information.
m.agg_info <- aggregation_information(timeshift = lubridate::hours(0),
                                      aggregation_interval = '1 day')
print(m.agg_info)



# Data gathering ----------------------------------------------------------
m.date_pattern <- unique(substring(as.character(seq(as.Date(start_date),
                                             as.Date(end_date), 1)), 1,7))
#' Collection the Informations about the sensors and the collected data from
#' the sensors in the choosen area.
sensors <- get_sensors(date_pattern = m.date_pattern, plz = m.plz, path = path)
sensor_age <- get_sensor_age(path = path)
sensor_data <- get_sensor_measured_values(sensors, m.date_pattern, path = path)

# Traffic data ------------------------------------------------------------
#' Please note, if you want to gather data for a big area, I suggest to use a
#' fixed value for lambda, e.g. lambda = 0.1, because the optimisation of
#' optim_lambda() will take a huge amount of computation costs.
#' lambda.p1 <- lambda.p2 <- 0.1
estimate_grid_size(m.plz)
roads <- get_opentransportmap_data(m.plz, path = path, trafficvol_treshold = 1)
lambda.p1 <- optim_lambda(sensor_data[['P1']], sensors[['P1']], roads = roads,
                          validation_plot = TRUE)
lambda.p2 <- optim_lambda(sensor_data[['P2']], sensors[['P2']], roads = roads,
                          validation_plot = TRUE)
grid.traffic.p1 <- make_grid_traffic(lambda.p1, m.plz)
grid.traffic.p2 <- make_grid_traffic(lambda.p2, m.plz)
data.traffic.p1 <- make_data_traffic(sensors = sensors[['P1']], lambda = lambda.p1)
data.traffic.p2 <- make_data_traffic(sensors = sensors[['P2']], lambda = lambda.p2)

# Space Time data ---------------------------------------------------------
calculate_space_time_datasets()

# Time Variables ----------------------------------------------------------
calculate_time_datasets()

# Rename datasets ---------------------------------------------------------
rename_datasets()

# Combine all calculated datasets -----------------------------------------
combine_datasets()


# Save RDS files ----------------------------------------------------------
saveRDS(data.final.p1, file = paste0(path, '/p1_model_data_',m.grid_cellsize, '.rds'))
saveRDS(grid.final.p1, file = paste0(path, '/p1_grid_data_',m.grid_cellsize, '.rds'))
saveRDS(data.final.p2, file = paste0(path, '/p2_model_data_',m.grid_cellsize, '.rds'))
saveRDS(grid.final.p2, file = paste0(path, '/p2_grid_data_',m.grid_cellsize, '.rds'))


