raw_plot <- function(.df, .out, .time, .phase = NULL, .cond = NULL,
                     .participant = NULL, phase_levels = NULL, phase_labels = NULL,
                     cond_levels = NULL, cond_labels = NULL){

  # Set local variables
  .data <- NULL

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
    # Split data by participant
    if (!is.null(.participant)) {
      split_data <- split(.df, f = .df[[.participant]])
    } else {
      split_data <- list(.df)
    }

    # Create annotations data frame for each participant
    annotations_list <- lapply(split_data, function(participant_data) {
      phase_changes <- which(diff(as.numeric(factor(participant_data[[.phase]]))) != 0)
      midpoints <- participant_data$time[phase_changes] + 0.5
      start_points <- c(1, midpoints + 0.5)
      end_points <- c(midpoints - 0.5, max(participant_data$time))
      data.frame(
        label = levels(factor(participant_data[[.phase]])),
        x = (start_points + end_points) / 2,
        y = rep(max(participant_data[[.out]]) * 1.05, length(start_points)), # Adjust Y position as needed
        .participant = unique(participant_data[[.participant]])
      )
    })

    annotations_df <- do.call(rbind, annotations_list)

    # Add phases and styling to plot
    rawPlot <- rawPlot +
      ggplot2::geom_vline(
        data = annotations_df,
        ggplot2::aes(xintercept = x, group = .participant),
        linetype = "dashed",
        size = 1.2
      ) +
      ggplot2::geom_text(
        data = annotations_df,
        ggplot2::aes(x = x, y = y, label = label, group = .participant),
        inherit.aes = FALSE
      ) +
      ggplot2::theme_minimal() +
      ggplot2::ylab(.out) +
      ggplot2::xlab("time") +
      ggplot2::theme(axis.ticks.x = ggplot2::element_blank(),
                     legend.title = ggplot2::element_blank())
  }

  if (!is.null(.participant)) {
    rawPlot <- rawPlot + ggplot2::facet_grid(.participant ~ .)
  }

  # # Get phase plot
  # if (!is.null(.phase)){
  #   # Identify where phase changes occur and calculate midpoints
  #   phase_changes <- which(diff(as.numeric(factor(.df[[.phase]]))) != 0)
  #   midpoints <- .df$time[phase_changes] + .5
  #   start_points <- c(1, midpoints + 0.5)
  #   end_points <- c(midpoints - 0.5, max(.df$time))
  #
  #   # Prepare a data frame for annotations
  #   annotations_df <- data.frame(label = levels(factor(.df[[.phase]])),
  #                                x = (start_points + end_points) / 2,
  #                                y = rep(max(.df[[.out]]) * 1.05, length(start_points))) # Adjust Y position as needed
  #
  #   # Add phases and styling to plot
  #   rawPlot <- rawPlot +
  #     ggplot2::geom_vline(xintercept = midpoints,
  #                         linetype = "dashed",
  #                         size= 1.2)+
  #     ggplot2::geom_text(data = annotations_df, ggplot2::aes(x = x, y = y, label = label), inherit.aes = FALSE) +
  #     ggplot2::theme_minimal()+
  #     ggplot2::ylab(.out)+
  #     ggplot2::xlab("time")+
  #     ggplot2::theme(axis.ticks.x=ggplot2::element_blank(),
  #                    legend.title = ggplot2::element_blank())
  #
  # }
  #
  # if (!is.null(.participant)){
  #   rawPlot <- rawPlot + ggplot2::facet_grid(.participant)
  # }

  return(rawPlot)
}
