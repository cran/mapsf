% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mf_get_borders.R
\name{mf_get_borders}
\alias{mf_get_borders}
\title{Get a border layer from polygons}
\usage{
mf_get_borders(x)
}
\arguments{
\item{x}{an sf object of POLYGONS, using a projected CRS}
}
\value{
An sf object (MULTILINESTRING) of borders is returned.
}
\description{
This function extracts borders between contiguous polygons.
}
\note{
If the polygon layer contains topology errors (such as contiguous
polygons not sharing exactly the same boundary) the function may not return
all boundaries correctly. It is possible to use \code{st_snap()} or other
functions to try and correct these errors.
}
\examples{
mtq <- mf_get_mtq()
mtq_b <- mf_get_borders(mtq)
mf_map(mtq)
mf_map(mtq_b, col = 1:5, lwd = 4, add = TRUE)
}
