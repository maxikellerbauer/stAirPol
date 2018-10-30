require(stAirPol)
require(spTimer)

#' We need the lastest version of ggplot2
# devtools::install_github("tidyverse/ggplot2")
require(ggplot2)


# Load the data -----------------------------------------------------------
data("muc_airPol_p2")
data("muc_airPol_p2_grid")
muc_airPol_p2_grid[, sensor_id := .GRP, by = list(lon, lat)]
muc_airPol_p2_grid[, sensor_age := 1.1]
muc_airPol_p2_grid <- muc_airPol_p2_grid[timestamp != max(timestamp)]
data <- clean_model_data(muc_airPol_p2)
data <- data[timestamp != max(timestamp)]



# Estimate the PM values on a regular grid --------------------------------

#' For speeding up the computation we use the GPP model
formula = value ~ humi + temp + rainhist + windhist +
  trafficvol + log(sensor_age)
priors.gpp <- spT.priors(model = "GPP", inv.var.prior = Gamm(a = 2, b = 1),
                         beta.prior = Norm(0, 10^4))
spatial.decay = spT.decay(distribution = Gamm(a = 2, b = 1), tuning = 0.25)
scale.transform = "SQRT"
cov.fnc = "exponential"
model.gpp <- fit_sp_model(data = data,
                          formula = formula,
                          model = 'GPP',
                          priors = priors.gpp,
                          cov.fnc = cov.fnc,
                          knots_count = 25,
                          knots_method = 'random',
                          report = 5,
                          training_set = training_set,
                          scale.transform = scale.transform,
                          spatial.decay = spatial.decay)
#' Predict the gridpoints for each day
pred.gpp <- predict(model.gpp, muc_airPol_p2_grid)

# Aggregate the grid to a administrative level ----------------------------
#' Get a shapefile for munich
require(dplyr)
require(osmdata)
#' Load the shapefile from Open Street Map
q <- getbb("munich") %>%
  opq() %>%
  add_osm_feature("boundary", "administrative")
shape_muc <- osmdata_sf(q)$osm_multipolygons
#' retrict the shape file to 'Stadtbezirksteile'
shape_muc <- shape_muc[as.character(shape_muc$admin_level) == 10, ]
#' remove 'Bezirksteile'
shape_muc <- shape_muc[grepl('Bezirksteil', shape_muc$name), ]
#' Take a short look on the shapefile
ggplot(data = shape_muc) +
  geom_sf()
#' Now, we calculate which grid points is in what 'Stadtbezirksteil'
grid_coords <- unique(pred.gpp[, .(lon, lat, sensor_id)])
grid_coords$shape <- as.numeric(pbmcapply::pbmclapply(1:nrow(grid_coords),
                                                      function(i) {
  which(sf::st_intersects(sf::st_point(c(grid_coords[i]$lon,
                                         grid_coords[i]$lat)),
                          shape_muc$geometry, sparse = FALSE))
}, mc.cores = 4))
pred.gpp <- data.table(dplyr::left_join(pred.gpp, grid_coords))
#' Aggregate by 'Stadtbezirksteil'
pred.gpp.agg <- pred.gpp[, .(prediction = mean(prediction)), by = shape]
#' remove points which does not contain in the shapefile
pred.gpp.agg <- pred.gpp.agg[!is.na(shape)][order(shape)]
shape_muc$mean <- 0
shape_muc$mean[pred.gpp$shape] <- pred.gpp$prediction
#' remove shapes where no gridpoints are detected
shape_muc <- shape_muc[shape_muc$mean != 0, ]
#' Plot the calculated aggregation
g <- ggplot(data = shape_muc) +
  geom_sf(aes(fill = mean)) +
  coord_sf(crs = sf::st_crs(shape_muc), datum = NA) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1) +
  guides(fill = guide_colorbar(barwidth = 1.5, barheight = 20),
         shape = "colorbar") +
  theme_void() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.title = element_blank(),
        panel.background = element_blank()) +
  ggsn::scalebar(dist = 3, dd2km = TRUE, model = 'WGS84',
                 location = "bottomleft", st.dist = 0.05,
                 x.min = sf::st_bbox(shape_muc)[1],
                 y.min = sf::st_bbox(shape_muc)[2],
                 x.max = sf::st_bbox(shape_muc)[3],
                 y.max = sf::st_bbox(shape_muc)[4])

print(g)


# Only temporal aggregation -----------------------------------------------

pred.gpp.agg2 <- pred.gpp[, .(prediction = mean(prediction)),
                         by = list(lon, lat, sensor_id)]
g <- ggplot(data = pred.gpp.agg2, aes(x = lon, y = lat, fill = prediction)) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1) +
  geom_tile() +
  guides(fill = guide_colorbar(barwidth = 1.5, barheight = 20),
         shape = "colorbar") +
  theme_void() +
  ggsn::scalebar(dist = 3, dd2km = TRUE, model = 'WGS84',
                 location = "bottomleft", st.dist = 0.05,
                 x.min = min(pred.gpp.agg2$lon),
                 x.max = max(pred.gpp.agg2$lon),
                 y.min = min(pred.gpp.agg2$lat),
                 y.max = max(pred.gpp.agg2$lat))




