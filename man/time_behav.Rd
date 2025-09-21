\name{time_behav}
\alias{time_behav}
\title{
Calculate Behavioural Durations
}
\description{
Calculates the total time spent in different behaviours for each group of individuals,
based on time-stamped behavioural observations.
}
\usage{
time_behav(data, time_col, grouping, behav_col)
}
\arguments{
  \item{data}{
A data frame containing at least a time column, a grouping column (e.g. individual ID),
and a behaviour column.
}
  \item{time_col}{
Integer index of the column containing time information. This column will be converted to POSIXct if needed.
}
  \item{grouping}{
Integer index of the column containing the grouping variable (e.g. species ID or individual ID).
}
  \item{behav_col}{
Integer index of the column containing behaviour states.
}
}
\details{
The function orders the dataset by group and time, replaces any behaviour
values \code{"start"} or \code{"end"} with \code{"fly"}, computes the time
differences between consecutive observations, and sums these durations
for each group-behaviour combination. Results are reshaped into wide format,
with one column per behaviour.
}
\value{
A data frame where each row corresponds to one group, with columns
for each behaviour (time spent in seconds) and a \code{total_time} column.
}
\author{
Your Name
}
\examples{
library(dplyr)
library(tidyr)

df <- data.frame(
  id = c(1,1,1,2,2,2),
  time_obs = as.POSIXct("2025-09-21 10:00:00") + c(0, 60, 120, 0, 30, 60),
  behaviour = c("start", "rest", "end", "start", "fly", "end")
)

time_behav(df,
          time_col = 2,   # column "time_obs"
          grouping = 1,   # column "id"
          behav_col = 3)  # column "behaviour"
}
\keyword{behaviour}
\keyword{time}
