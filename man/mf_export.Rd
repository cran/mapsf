% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mf_export.R
\name{mf_export}
\alias{mf_export}
\title{Export a map}
\usage{
mf_export(
  x,
  filename = "map.png",
  width,
  height,
  res = 96,
  ...,
  expandBB = rep(0, 4)
)
}
\arguments{
\item{x}{object of class \code{sf}, \code{sfc} or \code{SpatRaster}}

\item{filename}{path to the exported file. If the file extention is ".png" a
png graphic device is opened, if the file extension is ".svg" a svg graphic
device is opened.}

\item{width}{width of the figure (pixels for png, inches for svg)}

\item{height}{height of the figure (pixels for png, inches for svg)}

\item{res}{resolution (for png)}

\item{...}{further parameters for png or svg export}

\item{expandBB}{fractional values to expand the bounding box with, in each
direction (bottom, left, top, right)}
}
\value{
No return value, a map file is initiated (in PNG or SVG format).
}
\description{
It is recommended to use \link{mf_svg} or \link{mf_png} instead of
\code{mf_export}.
\cr

Export a map with the extent of a spatial object.\cr
The map is exported in PNG or SVG format.\cr
If only one of \code{width} or \code{height} is set, \code{mf_export} uses
the width/height ratio of \code{x} bounding box to find a matching ratio for
the export.\cr
Always use \code{add = TRUE} in \code{mf_map} calls following an
\code{mf_export} call.\cr
Use \code{dev.off} to finish the export (see Examples).
}
\examples{
mtq <- mf_get_mtq()
(filename <- tempfile(fileext = ".png"))
mf_export(mtq, filename = filename)
mf_map(mtq, add = TRUE)
dev.off()
}
\keyword{internal}
