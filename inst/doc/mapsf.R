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
# set a theme
mf_theme("iceberg")
# plot a shadow
mf_shadow(mtq)
# plot municipalities
mf_map(mtq, type = "base", add = TRUE)
# layout
mf_layout(
  title = "Martinique",
  credits = paste0(
    "Sources: IGN, 2018\n",
    "mapsf ",
    packageVersion("mapsf")
  )
)

## ----mf_prop, message=FALSE, warning=FALSE------------------------------------
library(mapsf)
# import the sample data set
mtq <- mf_get_mtq()
# set a theme
mf_theme("darkula")
# plot a shadow
mf_shadow(mtq)
# plot municipalities
mf_map(mtq, add = TRUE)
# plot population
mf_map(
  x = mtq,
  var = "POP",
  type = "prop",
  inches = 0.25,
  col = "brown4",
  leg_pos = "topright",
  leg_adj = c(0, -2),
  leg_title = "Total population"
)
# layout
mf_layout(
  title = "Population Distribution in Martinique",
  credits = paste0(
    "Sources: Insee and IGN, 2018\n",
    "mapsf ",
    packageVersion("mapsf")
  )
)

## ----mf_map_c-----------------------------------------------------------------
library(mapsf)
# import the sample data set
mtq <- mf_get_mtq()
# population density (inhab./km2) using sf::st_area()
mtq$POPDENS <- 1e6 * mtq$POP / sf::st_area(mtq)
# set a theme
mf_theme("green")
# plot population density
mf_map(
  x = mtq,
  var = "POPDENS",
  type = "choro",
  breaks = "geom",
  nbreaks = 5,
  pal = "Greens",
  border = "white",
  lwd = 0.5,
  leg_pos = "topright",
  leg_title = "Population Density\n(people per km2)"
)
# layout
mf_layout(
  title = "Population Distribution in Martinique",
  credits = paste0(
    "Sources: Insee and IGN, 2018\n",
    "mapsf ",
    packageVersion("mapsf")
  )
)

## ----mf_map_t-----------------------------------------------------------------
library(mapsf)
# import the sample data set
mtq <- mf_get_mtq()
# set theme
mf_theme("dark")
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
# layout
mf_layout(
  title = "Administrative Status",
  credits = paste0(
    "Sources: Insee and IGN, 2018\n",
    "mapsf ",
    packageVersion("mapsf")
  )
)

## ----mf_map_pc, fig.width=5---------------------------------------------------
library(mapsf)
# import the sample data set
mtq <- mf_get_mtq()
# set theme
mf_theme("candy")
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
  leg_frame = TRUE
)
# layout
mf_layout(
  title = "Population & Wealth in Martinique, 2015",
  credits = paste0(
    "Sources: Insee and IGN, 2018\n",
    "mapsf ",
    packageVersion("mapsf")
  ),
  frame = TRUE
)

## ----mf_map_pt, fig.width=5---------------------------------------------------
library(mapsf)
# import the sample data set
mtq <- mf_get_mtq()
# set theme
mf_theme("ink")
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
# layout
mf_layout(
  title = "Population Distribution in Martinique",
  credits = paste0(
    "Sources: Insee and IGN, 2018\n",
    "mapsf ",
    packageVersion("mapsf")
  )
)

## ----mf_label-----------------------------------------------------------------
library(mapsf)
# import the sample data set
mtq <- mf_get_mtq()
# set theme
my_theme <- list(
  name = "mytheme",
  bg = "lightblue1",
  fg = "darkseagreen4",
  mar = c(0, 0, 0, 0),
  tab = TRUE,
  pos = "left",
  inner = TRUE,
  line = 1,
  cex = .9,
  font = 3
)
mf_theme(my_theme)
# plot municipalities
mf_map(mtq, col = "#e4e9de", border = "darkseagreen4")
# plot labels
mf_label(
  x = mtq,
  var = "LIBGEO",
  col = "black",
  cex = 0.7,
  font = 4,
  halo = TRUE,
  bg = "white",
  r = 0.1,
  overlap = FALSE,
  lines = FALSE
)
# layout
mf_layout(
  title = "Municipalities of Martinique",
  credits = paste0(
    "Sources: Insee and IGN, 2018\n",
    "mapsf ",
    packageVersion("mapsf")
  ),
  arrow = FALSE
)
# north arrow
mf_arrow(pos = "topright")

## ----mf_grad------------------------------------------------------------------
library(mapsf)
# import the sample data set
mtq <- mf_get_mtq()
# import the csv file embedded in mapsf
mob <- read.csv(system.file("csv/mob.csv", package = "mapsf"))
# Select links from Fort-de-France (97209))
mob_97209 <- mob[mob$i == 97209, ]
# create an sf object of links
mob_links <- mf_get_links(x = mtq, df = mob_97209)
# set theme
mf_theme("jsk")
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
  col = "red4",
  leg_frame = TRUE
)
# map layout
mf_layout(
  title = "Commuting to Fort-de-France",
  credits = paste0(
    "Sources: Insee and IGN, 2018\n",
    "mapsf ",
    packageVersion("mapsf")
  ),
  arrow = FALSE
)

## ----echo = FALSE-------------------------------------------------------------
mf_theme("default")

