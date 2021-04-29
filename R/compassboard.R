#
# mini_world <- st_read(system.file("gpkg/world.gpkg", package = "mapsf"),
#                       layer = "country", quiet = TRUE
# )
# or <- data.frame(id = "o", x = 2, y = 48)
# ds <- data.frame(id = c("NY", "LN", "N", "S", "E", "W"),
#                  x = c(-74, 0, 2, 2, 90 , -90),
#                  y = c(41, 52, 80, -80, 48, 48))
# pstr <- paste0("+proj=aeqd +lat_0=", or$y, " +lon_0=",or$x)
# pstr <- "+proj=merc +lon_0=156 +lat_0=35 +k=1 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs "
#
# or <- st_as_sf(or, coords = c("x", "y"), remove = FALSE, crs = 4326)
# ds <- st_as_sf(ds, coords = c("x", "y"), remove = FALSE, crs = 4326)
#
# or <- st_transform(or, pstr)
# ds <- st_transform(ds, pstr)
#
#
# x <- st_transform(mini_world, pstr)
#
#
# plot(x$geom)
#
#
# plot(or$geometry, add = T, pch = 16, col = "red")
# plot(ds$geometry, add = T, pch = 16, col = "blue")
#
#
#
