% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/package.R
\docType{package}
\name{mapsf}
\alias{mapsf-package}
\alias{mapsf}
\title{Package description}
\description{
Create and integrate thematic maps in your workflow. This package
helps to design various cartographic representations such as proportional
symbols, choropleth or typology maps. It also offers several functions to
display layout elements that improve the graphic presentation of maps
(e.g. scale bar, north arrow, title, labels). \code{mapsf} maps \code{sf} objects on
\code{base} graphics.

A "Get Started" \strong{vignette} contains commented scripts on how to create
various maps: \code{vignette(topic = "mapsf", package = "mapsf")}
}
\section{Symbology}{

These functions display cartographic layers.
\itemize{
\item \code{\link[=mf_map]{mf_map()}} Plot a map
\item \code{\link[=mf_label]{mf_label()}} Plot labels
\item \code{\link[=mf_raster]{mf_raster()}} Plot a raster
\item \code{\link[=mf_graticule]{mf_graticule()}} Plot graticules
}
}

\section{Map layout}{

These functions are dedicated to the map layout design.
\itemize{
\item \code{\link[=mf_theme]{mf_theme()}} Set a theme
\item \code{\link[=mf_shadow]{mf_shadow()}} Plot a shadow
\item \code{\link[=mf_background]{mf_background()}} Plot a background image
\item \code{\link[=mf_annotation]{mf_annotation()}} Plot an annotation
\item \code{\link[=mf_arrow]{mf_arrow()}} Plot a north arrow
\item \code{\link[=mf_credits]{mf_credits()}} Plot credits
\item \code{\link[=mf_layout]{mf_layout()}} Plot a map layout
\item \code{\link[=mf_title]{mf_title()}} Plot a title
\item \code{\link[=mf_scale]{mf_scale()}} Plot a scale bar
\item \code{\link[=mf_inset_on]{mf_inset_on()}} / \code{\link[=mf_inset_off]{mf_inset_off()}} Plot an inset
\item \code{\link[=mf_worldmap]{mf_worldmap()}} Plot a point on a world map
\item \code{\link[=mf_legend]{mf_legend()}} Plot a legend
}
}

\section{Utility functions}{

\itemize{
\item \code{\link[=mf_svg]{mf_svg()}} Export a map in SVG file format
\item \code{\link[=mf_png]{mf_png()}} Export a map in SVG file format
\item \code{\link[=mf_distr]{mf_distr()}} Plot a distribution
\item \code{\link[=mf_get_links]{mf_get_links()}} Get a link layer from a data.frame of links
\item \code{\link[=mf_get_pal]{mf_get_pal()}} Get color palettes
\item \code{\link[=mf_get_breaks]{mf_get_breaks()}} Get class intervals
\item \code{\link[=mf_get_mtq]{mf_get_mtq()}} Get the 'mtq' dataset
\item \code{\link[=mf_get_ratio]{mf_get_ratio()}} Get map width and height values
\item \code{\link[=mf_get_pencil]{mf_get_pencil()}} Get a pencil layer from polygons
\item \code{\link[=mf_get_borders]{mf_get_borders()}} Get a border layer from polygons
}
}

\seealso{
Useful links:
\itemize{
  \item \url{https://riatelab.github.io/mapsf/}
  \item Report bugs at \url{https://github.com/riatelab/mapsf/issues/}
}

}
\author{
\strong{Maintainer}: Timothée Giraud \email{timothee.giraud@cnrs.fr} (\href{https://orcid.org/0000-0002-1932-3323}{ORCID})

Other contributors:
\itemize{
  \item Hugues Pecout (\href{https://orcid.org/0000-0002-0246-0954}{ORCID}) (Logo) [contributor]
  \item Ronan Ysebaert (\href{https://orcid.org/0000-0002-7344-5911}{ORCID}) (Cheat sheet) [contributor]
  \item Elina Marveaux (\href{https://orcid.org/0009-0000-8667-3019}{ORCID}) (Themes) [contributor]
  \item Ian Fellows (No overlap algorithm for labels, from wordcloud package) [copyright holder]
  \item Jim Lemon (Arc drawing algorithm for annotations, from plotrix package) [copyright holder]
}

}
