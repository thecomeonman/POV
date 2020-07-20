% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fGetTransformedCoordinates.R
\name{fGetTransformedCoordinates}
\alias{fGetTransformedCoordinates}
\title{Transforming coordinate spaces}
\usage{
fGetTransformedCoordinates(
  mCoordinates,
  mOrigin,
  mScreenCoordinate,
  iTreatAs = 3
)
}
\arguments{
\item{mCoordinates}{matrix with three columns - x,y,z coorinates.
if mCoordinates is an isolated point then it is treated
as an isolated point, if it is two points, it is treated as a line segment,
but 3 or more points are treated as closed polygons unless iTreatAs = F.
Recommendation is to ensure start point is repeated as end point in mCoordinates
and iTreatAs = F. That way, where the polygon / path went behind the
screen, there will be a marker specifying that the line has to be broken there
when plotting or anywhere else}

\item{mOrigin}{three column, one row matrix specifying coordinates of where
the scene is being viewed from}

\item{mScreenCoordinate}{three column, one row matrix specifying coordinates of
where the screen on which the scene is being projected on. Also dictates the
direction along which which the scene is being viewed from}

\item{iTreatAs}{1 for isolated points, 2 for path, 3 for polygon}
}
\value{
a matrix with 2 columns for the x,y coordinates on the screen + an
optional column linking continuous stretches in front of the screening plane
which can be used in geom_path(aes(group = V3))
}
\description{
Transforming coordinate spaces
}