flight_summary <- function(data, time, xcoord, ycoord) {
  ### number of data for each speciment
  numb_points <- nrow(data)

  data <- data %>%
    rename(
      time = !!time,
      x = !!xcoord,
      y = !!ycoord
    ) %>%
    mutate(time = ymd_hms(time)) %>%
    mutate(
      seconds = hour(time) * 3600 +
        minute(time) * 60 +
        second(time)
    ) %>%
    # Rendi il tempo relativo al primo valore
    mutate(seconds = seconds - min(seconds)) %>%
    mutate(
      x = as.numeric(x),
      y = as.numeric(y),
      time = as.numeric(seconds)
    ) %>%
    select(x, y, time)


  ### TrajR can't work with less then 3 points
  if (nrow(data) < 3) {
    return(data.frame(
      sinuosity = NA,
      length = NA,
      distance = NA,
      duration = NA,
      velocity = NA,
      mean_velocity = NA,
      mean_vect_turningangle = NA
    ))
  }

  # Crea la traiettoria
  TRJ <- TrajFromCoords(data,
                        xCol = 1,
                        yCol = 2,
                        timeCol = 3,
                        spatialUnits = "m",
                        timeUnits = "s")

  # Calcola i parametri
  res <- data.frame(
    sinuosity = TrajSinuosity2(TRJ),
    length = TrajLength(TRJ),
    distance = TrajDistance(TRJ),
    duration = TrajDuration(TRJ),
    mean_velocity = TrajMeanVelocity(TRJ),
    mean_vect_turningangle = TrajMeanVectorOfTurningAngles(TRJ)
  )
  return(res)
}
