## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 5,
  fig.height = 6
)

## ----mf_basemap, message=FALSE, warning=FALSE---------------------------------
library(mapsf)
# import the sample data set
mtq <- mf_get_mtq()
# plot municipalities
mf_map(mtq, type = "base")
# layout elements
credits <- paste0("Sources: IGN, 2018\n", "mapsf ", packageVersion("mapsf"))
mf_title("Martinique")
mf_credits(credits)
mf_arrow()
mf_scale()

## ----mf_prop, message=FALSE, warning=FALSE------------------------------------
# plot municipalities
mf_map(mtq)
# plot population
mf_map(
  x = mtq,
  var = "POP",
  type = "prop",
  inches = 0.25,
  col = "brown4",
  leg_pos = "topright",
  leg_adj = c(0, -3),
  leg_title = "Total population"
)
# layout elements
mf_title("Population Distribution in Martinique")
mf_credits(credits)
mf_arrow()
mf_scale()

## ----mf_map_c-----------------------------------------------------------------
# population density (inhab./km2) using sf::st_area()
mtq$POPDENS <- 1e6 * mtq$POP / sf::st_area(mtq)
# plot population density
mf_map(
  x = mtq,
  var = "POPDENS",
  type = "choro",
  breaks = "geom",
  nbreaks = 5,
  pal = "Teal",
  border = "white",
  lwd = 0.5,
  leg_pos = "bottomleft",
  leg_adj = c(0, 3),
  leg_title = "Population Density\n(inh. / km2)"
)
# layout elements
mf_title("Population Distribution in Martinique")
mf_credits(credits)
mf_arrow()
mf_scale()

## ----mf_map_t-----------------------------------------------------------------
# plot administrative status
mf_map(
  x = mtq,
  var = "STATUS",
  type = "typo",
  pal = c("aquamarine4", "yellow3", "wheat"),
  lwd = .5,
  val_order = c(
    "Prefecture",
    "Sub-prefecture",
    "Simple municipality"
  ),
  leg_pos = "topright",
  leg_adj = c(0, 1),
  leg_title = ""
)
# labels for a few  municipalities
mf_label(
  x = mtq[mtq$STATUS != "Simple municipality", ], var = "LIBGEO",
  cex = 0.9, halo = TRUE, r = 0.15
)
# layout elements
mf_title("Administrative Status")
mf_credits(credits)
mf_arrow()
mf_scale()

## ----mf_map_pc, fig.width=5---------------------------------------------------
# Plot the municipalities and expand the map space on the right
mf_map(x = mtq, expandBB = c(0, 0, 0, .15))
# Plot symbols with choropleth coloration
mf_map(
  x = mtq,
  var = c("POP", "MED"),
  type = "prop_choro",
  border = "grey50",
  lwd = 1,
  leg_pos = c("topright"),
  leg_title = c("Population", "Median Income\n(in euros)"),
  breaks = "equal",
  nbreaks = 4,
  pal = "Greens",
  leg_val_rnd = c(0, -2),
  leg_frame = FALSE
)
# layout elements
mf_title("Population & Wealth in Martinique, 2015")
mf_credits(credits)
mf_arrow()
mf_scale()

## ----mf_map_pt, fig.width=5---------------------------------------------------
# plot the municipalities and expand the map space on the right
mf_map(x = mtq, expandBB = c(0, 0, 0, .15))
# plot symbols with choropleth coloration
mf_map(
  x = mtq,
  var = c("POP", "STATUS"),
  type = "prop_typo",
  symbol = "square",
  border = "white",
  lwd = .5,
  leg_pos = "topright",
  leg_title = c("Population", "Administrative\nStatus"),
  val_order = c(
    "Prefecture", "Sub-prefecture",
    "Simple municipality"
  )
)
# layout elements
mf_title("Population Distribution in Martinique")
mf_credits(credits)
mf_arrow()
mf_scale()

## ----mf_label-----------------------------------------------------------------
# plot municipalities
mf_map(mtq, col = "#e4e9de", border = "darkseagreen4")
# plot labels
mf_label(
  x = mtq,
  var = "LIBGEO",
  cex = 0.7,
  font = 4,
  halo = TRUE,
  r = 0.1,
  overlap = FALSE,
  q = 3,
  lines = FALSE
)
# layout elements
mf_title("Municipalities of Martinique")
mf_credits(credits)
mf_arrow(pos = "topright")
mf_scale()

## ----mf_grad------------------------------------------------------------------
# import the csv file embedded in mapsf
mob <- read.csv(system.file("csv/mob.csv", package = "mapsf"))
# Select links from Fort-de-France (97209))
mob_97209 <- mob[mob$i == 97209, ]
# create an sf object of links
mob_links <- mf_get_links(x = mtq, df = mob_97209)
# Plot the municipalities
mf_map(mtq)
# plot graduated links
mf_map(
  x = mob_links,
  var = "fij",
  type = "grad",
  breaks = c(100, 500, 1000, 4679.0),
  lwd = c(1, 4, 8),
  leg_pos = "topright",
  leg_title = "Nb. of\nCommuters",
  leg_val_rnd = 0
)
# layout elements
mf_title("Commuting to Fort-de-France")
mf_credits(credits)
mf_arrow()
mf_scale()

