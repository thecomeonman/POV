% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fGetTransformedCoordinates.R
\name{fGetTransformedCoordinates}
\alias{fGetTransformedCoordinates}
\title{Function to easily transform coordinate spaces}
\usage{
fGetTransformedCoordinates(
  mCoordinates,
  mOriginCoordinates,
  mScreenCoordinates,
  mViewBeginsFromCoordinates = NULL,
  mVectorPointingUpOnScreen = c(0, 0, 1),
  iTreatAs = 3
)
}
\arguments{
\item{mCoordinates}{matrix with three columns [x,y,z] with >= one rows}

\item{mOriginCoordinates}{three column, one row matrix specifying coordinates
of where the scene is being viewed from}

\item{mScreenCoordinates}{three column, one row matrix specifying coordinates
of the screen on which the scene is being projected on. The function
calculates a plane which is perpendicular to the vector from mOriginCoordinates
to mScreenCoordinates which contains mScreenCoordinates. This plane is the
screen on which things are projected finally.}

\item{mViewBeginsFromCoordinates}{if NULL, all objects in front of origin, where
front is the direciton in which the screen coordinates are, are projected. If
a set of coordinates is given then all objects in front of that coordinate, where
front is the direciton in which the screen coordinates are, are retained, and
other objects are treated as behind the view. If this coordinate is on the opposite
side of the origin as the screen then you'll delete all objects and get an empty
array in the results. This dividing plane and iTreatAs together also affect
how points are treated when data is crossing the dividing plane. Points behind the
dividing plane are just deleted but paths / polygons get an interpolated coordinate
on the dividing plane which helps retain a continuity to the data when plotted}

\item{mVectorPointingUpOnScreen}{Which way is up for the viewer}

\item{iTreatAs}{1 for isolated points, 2 for path, 3 for polygon. This matters
if you have data going behind the mOriginCoordinates / mViewBeginsFromCoordinates
as explained in mViewBeginsFromCoordinates.}
}
\value{
If inputs are valid, a matrix with 2 columns for the x,y coordinates
on the screen, an optional column, group: linking continuous stretches in front of
the screening plane which can be used in geom_path(aes(group = group)), an optional
column inputOrder: which lets you map the output back to the data that was sent
in. If invalid inputs then you'll get a NULL.
}
\description{
Function to easily transform coordinate spaces
}
