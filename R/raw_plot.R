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
  rawPlot <- ggplot2::ggplot(.df, ggplot2::aes(x = .data[[.time]], y = .data[[.out]], color = .data[[.cond]])) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::theme_minimal() +
    ggplot2::ggtitle("Raw Data by Condition")

  return(rawPlot)
}
