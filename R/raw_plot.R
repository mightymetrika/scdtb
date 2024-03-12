raw_plot <- function(.df, .out, .time, .phase = NULL, .cond = NULL,
                     .participant = NULL, phase_levels = NULL, phase_labels = NULL,
                     cond_levels = NULL, cond_labels = NULL, label_raise = 2){

  # Set local variables
  .data <- NULL
  label <- NULL
  x <- NULL
  y <- NULL

  # Initial parameter checks
  if(!is.character(.out))stop(".out must be an object of type character")
  if(!is.character(.time))stop(".time must be an object of type character")
  if(!is.data.frame(.df)) stop(".df must be a data frame")

  # Set factor levels and labels of the condition variable
  if(!is.null(.cond)){
    if(!(.cond %in% names(.df))) stop(".cond must be a variable in .df")

    .df <- process_cond(.df, .cond, cond_levels, cond_labels)
  }

  # Set factor levels and labels of the phase variable
  if(!is.null(.phase)){
    if(!is.character(.phase))stop(".phase must be an object of type character")

    .df <- process_phase(.df, .phase, phase_levels, phase_labels)
  }

  # Get raw data plot
  if(!is.null(.cond)){
    rawPlot <- ggplot2::ggplot(.df, ggplot2::aes(x = .data[[.time]], y = .data[[.out]], color = .data[[.cond]])) +
      ggplot2::geom_point() +
      ggplot2::geom_line() +
      ggplot2::theme_minimal()
  } else {
    rawPlot <- ggplot2::ggplot(.df, ggplot2::aes(x = .data[[.time]], y = .data[[.out]])) +
      ggplot2::geom_point() +
      ggplot2::geom_line() +
      ggplot2::theme_minimal()
  }

  # Get phase plot
  if (!is.null(.phase)) {

    out_offset <- mean(diff(.df[[.out]], na.rm = TRUE))
    out_max <- max(.df[[.out]], na.rm = TRUE)

    # Split data by participant
    if (!is.null(.participant)) {
      split_data <- split(.df, f = .df[[.participant]])

      # Create annotations data frame for each participant
      annotations_list <- lapply(split_data, function(participant_data) {
        phase_changes <- which(diff(as.numeric(factor(participant_data[[.phase]]))) != 0)

        midpoints <- participant_data$time[phase_changes]
        offset <- mean(diff(participant_data[[.time]]), na.rm = TRUE)/2

        start_points <- c(min(participant_data$time), midpoints + offset)

        data.frame(
          label = levels(factor(participant_data[[.phase]])),
          x = midpoints + offset,
          y = rep(max(participant_data[[.out]]) * 1.05, length(start_points)), # Adjust Y position as needed
          .participant = unique(participant_data[[.participant]])
        )
      })

      annotations_df <- do.call(rbind, annotations_list)

      # Create annotations data frame for each participant
      labels_list <- lapply(split_data, function(participant_data) {
        phase_changes <- which(diff(as.numeric(factor(participant_data[[.phase]]))) != 0)


        midpoints <- participant_data$time[phase_changes]
        offset <- mean(diff(participant_data[[.time]]), na.rm = TRUE)/2

        start_points <- c(min(participant_data$time), midpoints + offset)
        end_points <- c(midpoints - offset, max(participant_data$time))

        data.frame(
          label = levels(factor(participant_data[[.phase]])),
          x = (start_points + end_points) / 2,
          y = rep(out_max + label_raise*out_offset, length(start_points)), # Adjust Y position as needed
          .participant = unique(participant_data[[.participant]])
        )
      })

      labels_df <- do.call(rbind, labels_list)

      # Add phases and styling to plot
      rawPlot <- rawPlot +
        ggplot2::geom_vline(
          data = annotations_df,
          ggplot2::aes(xintercept = x, group = .participant),
          linetype = "dashed"
        ) +
        ggplot2::geom_text(
          data = labels_df,
          ggplot2::aes(x = x, y = y, label = label, group = .participant),
          inherit.aes = FALSE
        ) +
        ggplot2::theme_minimal() +
        ggplot2::ylab(.out) +
        ggplot2::xlab("time") +
        ggplot2::theme(axis.ticks.x = ggplot2::element_blank(),
                       legend.title = ggplot2::element_blank())
    } else {

      # Create annotations data frame
        phase_changes <- which(diff(as.numeric(factor(.df[[.phase]]))) != 0)

        midpoints <- .df$time[phase_changes]
        offset <- mean(diff(.df[[.time]]), na.rm = TRUE)/2

        start_points <- c(min(.df$time), midpoints + offset)

        annotations_df <- data.frame(
          label = levels(factor(.df[[.phase]])),
          x = midpoints + offset,
          y = rep(max(.df[[.out]]) * 1.05, length(start_points))
        )

      # Create annotations data frame
        phase_changes <- which(diff(as.numeric(factor(.df[[.phase]]))) != 0)


        midpoints <- .df$time[phase_changes]
        offset <- mean(diff(.df[[.time]]), na.rm = TRUE)/2

        start_points <- c(min(.df$time), midpoints + offset)
        end_points <- c(midpoints - offset, max(.df$time))

        labels_df <- data.frame(
          label = levels(factor(.df[[.phase]])),
          x = (start_points + end_points) / 2,
          y = rep(out_max + label_raise*out_offset, length(start_points)) # Adjust Y position as needed
        )

      # Add phases and styling to plot
      rawPlot <- rawPlot +
        ggplot2::geom_vline(
          data = annotations_df,
          ggplot2::aes(xintercept = x),
          linetype = "dashed"
        ) +
        ggplot2::geom_text(
          data = labels_df,
          ggplot2::aes(x = x, y = y, label = label),
          inherit.aes = FALSE
        ) +
        ggplot2::theme_minimal() +
        ggplot2::ylab(.out) +
        ggplot2::xlab("time") +
        ggplot2::theme(axis.ticks.x = ggplot2::element_blank(),
                       legend.title = ggplot2::element_blank())

    }

  }

  if (!is.null(.participant)) {
    rawPlot <- rawPlot + ggplot2::facet_grid(.participant ~ .)
  }

  return(rawPlot)
}
