\name{flight_summary}
\alias{flight_summary}
\title{
Flight Summary
}
\description{
Summarizes movement trajectories from coordinate and time data.
Requires the \pkg{TrajR} package.
}
\usage{
flight_summary(data, time, xcoord, ycoord)
}
\arguments{
  \item{data}{
A data frame containing at least a time column, an x coordinate column, and a y coordinate column.
}
  \item{time}{
Character string, the name of the column containing time information.
}
  \item{xcoord}{
Character string, the name of the column containing x coordinates.
}
  \item{ycoord}{
Character string, the name of the column containing y coordinates.
}
}
\details{
The function converts time into seconds relative to the first observation,
creates a trajectory object using \code{\link[TrajR]{TrajFromCoords}}, and computes
basic movement metrics.
}
\value{
A data frame with the following components:
  \item{sinuosity}{Sinuosity of the trajectory.}
  \item{length}{Total trajectory length.}
  \item{distance}{Straight-line distance between start and end.}
  \item{duration}{Duration in seconds.}
  \item{mean_velocity}{Mean velocity across steps.}
  \item{mean_vect_turningangle}{Mean vector of turning angles.}
}
\author{
Your Name
}
\note{
This function will return \code{NA} values if fewer than 3 points are provided.
}
\examples{
library(TrajR)

# Example with simulated data
df <- data.frame(
  time = seq.POSIXt(from = as.POSIXct("2025-09-21 10:00:00"),
                    by = "1 min", length.out = 10),
  xcoord = runif(10, 0, 100),
  ycoord = runif(10, 0, 100)
)

flight_summary(df, time = "time",
                   xcoord = "xcoord",
                   ycoord = "ycoord")
}
\keyword{trajectory}
\keyword{movement}
