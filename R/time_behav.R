time_behav <- function(data, time_col, grouping, behav_col) {

  # convert indices to names
  time_col <- names(data)[time_col]
  grouping <- names(data)[grouping]
  behav_col <- names(data)[behav_col]

  data %>%
    # ensure proper ordering
    arrange(.data[[grouping]], .data[[time_col]]) %>%
    group_by(.data[[grouping]]) %>%
    mutate(
      # convert time to POSIXct if not already
      !!time_col := as.POSIXct(.data[[time_col]], tz = "UTC"),
      # replace "start"/"end" with "fly"
      !!behav_col := if_else(.data[[behav_col]] %in% c("start", "end"),
                             "fly", .data[[behav_col]]),
      next_time = lead(.data[[time_col]]),
      duration = as.numeric(difftime(next_time, .data[[time_col]], units = "secs"))
    ) %>%
    ungroup() %>%
    group_by(.data[[grouping]], .data[[behav_col]]) %>%
    summarise(time_sec = sum(duration, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(
      names_from = !!sym(behav_col),
      values_from = time_sec,
      values_fill = 0
    ) %>%
    rowwise() %>%
    mutate(total_time = sum(c_across(where(is.numeric)), na.rm = TRUE)) %>%
    ungroup()
}

