#' Plot Raw Data with Optional Phase and Condition Annotations
#'
#' This function generates a plot of raw data from a specified data frame. It
#' supports optional annotations based on phase and condition variables, and can
#' facet the plot by participant. The plot is customizable with parameters for
#' setting factor levels and labels for both phase and condition variables. It
#' utilizes ggplot2 for plotting.
#'
#' @param .df A data frame containing the data to be plotted. Must contain columns
#'            specified by .time, .out, and optionally .phase, .cond, and .participant
#'            if used.
#' @param .out The name of the column in .df that contains the outcome variable
#'             to be plotted on the y-axis.
#' @param .time The name of the column in .df that contains the time variable to
#'              be plotted on the x-axis.
#' @param .phase (Optional) The name of the column in .df that contains the phase
#'               variable used for annotating the plot with phase changes. If NULL,
#'               phase annotations are not added.
#' @param .cond (Optional) The name of the column in .df that contains the condition
#'               variable. If not NULL, data points are colored based on condition.
#' @param .participant (Optional) The name of the column in .df that contains
#'                     participant identifiers. If not NULL, the plot is faceted
#'                     by participant.
#' @param phase_levels (Optional) A vector of values indicating the order of phase
#'                     levels. This is used to set the factor levels of the phase
#'                     variable.
#' @param phase_labels (Optional) A vector of labels corresponding to the phase
#'                     levels. These labels are used in annotations.
#' @param cond_levels (Optional) A vector of values indicating the order of condition
#'                    levels. This is used to set the factor levels of the condition
#'                    variable.
#' @param cond_labels (Optional) A vector of labels corresponding to the condition
#'                    levels. These are used for the legend.
#' @param label_raise A numeric value indicating how much to raise the phase labels
#'                    on the y-axis. Defaults to 2.
#'
#' @return A ggplot object representing the raw data plot with optional annotations
#'         for phase and condition, and faceting by participant if specified.

#' @examples
#' rp <- raw_plot(.df = efficacy_of_CBT, .out = "Anxious", .time = "time",
#'                .phase = "phase", phase_levels = c(0, 1),
#'                phase_labels = c("Exposure", "Exposure + CT"))
#'
#' @export
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

  # Get raw data plot
  if(!is.null(.cond)){

    # Set factor levels and labels of the condition variable
    .df <- process_cond(.df, .cond, cond_levels, cond_labels)

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

    # Set factor levels and labels of the phase variable
    .df <- process_phase(.df, .phase, phase_levels, phase_labels)

    # Helper function for processing phase changes
    process_phase_change <- function(.data){
      phase_changes <- which(diff(as.numeric(factor(.data[[.phase]]))) != 0)
      midpoints <- .data[[.time]][phase_changes]
      offset <- mean(diff(.data[[.time]]), na.rm = TRUE)/2
      start_points <- c(min(.data[[.time]]), midpoints + offset)
      end_points <- c(midpoints - offset, max(.data[[.time]]))

      return(list(phase_changes = phase_changes,
                  midpoints = midpoints,
                  offset = offset,
                  start_points = start_points,
                  end_points = end_points))
    }

    # Set output reference values
    out_offset <- mean(diff(.df[[.out]], na.rm = TRUE))
    out_max <- max(.df[[.out]], na.rm = TRUE)

    # Split data by participant
    if (!is.null(.participant)) {
      if(!(.participant %in% names(.df))) stop(".participant must be a variable in .df")

      split_data <- split(.df, f = .df[[.participant]])

      # Create annotations data frame for each participant
      annotations_list <- lapply(split_data, function(participant_data) {

        # Process phase change
        .pc <- process_phase_change(participant_data)

        # Create midpoint annotation data frame
        data.frame(
          label = levels(factor(participant_data[[.phase]])),
          x = .pc$midpoints + .pc$offset,
          y = rep(max(participant_data[[.out]]) * 1.05, length(.pc$start_points)), # Adjust Y position as needed
          .participant = unique(participant_data[[.participant]])
        )
      })

      # Combine midpoint annotation data frames across participants
      annotations_df <- do.call(rbind, annotations_list)

      # Create annotations data frame for each participant
      labels_list <- lapply(split_data, function(participant_data) {

        # Process phase change
        .pc <- process_phase_change(participant_data)

        # Create phase label data frame
        data.frame(
          label = levels(factor(participant_data[[.phase]])),
          x = (.pc$start_points + .pc$end_points) / 2,
          y = rep(out_max + label_raise*out_offset, length(.pc$start_points)), # Adjust Y position as needed
          .participant = unique(participant_data[[.participant]])
        )
      })

      # Combine phase label data frames across participants
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

      # Process phase change data
      .pc <- process_phase_change(.df)

      # Create midpoint annotation data frame
      annotations_df <- data.frame(
        label = levels(factor(.df[[.phase]])),
        x = .pc$midpoints + .pc$offset,
        y = rep(max(.df[[.out]]) * 1.05, length(.pc$start_points))
        )

      # Create phase labels data frame
      labels_df <- data.frame(
        label = levels(factor(.df[[.phase]])),
        x = (.pc$start_points + .pc$end_points) / 2,
        y = rep(out_max + label_raise*out_offset, length(.pc$start_points)) # Adjust Y position as needed
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

  # Facet plots by participants
  if (!is.null(.participant)) {
    rawPlot <- rawPlot + ggplot2::facet_grid(.participant ~ .)
  }

  return(rawPlot)
}
