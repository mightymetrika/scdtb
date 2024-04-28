#' Mixed Model Analysis
#'
#' Performs a mixed model analysis on a dataset, allowing for the specification
#' of a dependent variable, time variable, phase variable, participant
#' identification, and covariates. It supports reverse timing within phases,
#' custom phase levels and labels, and adds covariates to the fixed effects model.
#' The function fits a model using generalized least squares and returns
#' a list containing the modified dataset, the fitted model, and a plot of
#' predicted values with phase annotations.
#'
#' @param .df A data frame containing the dataset to be analyzed.
#' @param .dv The name of the dependent variable in the dataset.
#' @param .time The name of the time variable in the dataset.
#' @param .phase The name of the phase variable in the dataset.
#' @param .participant (optional) The name of the participant identifier variable
#'         in the dataset. If not provided, a default factor will be used.
#' @param rev_time_in_phase (optional) A boolean indicating whether to reverse
#'        the timing within each phase. Defaults to FALSE.
#' @param phase_levels (optional) A vector of phase levels to be used for the phase
#'        variable. If NULL, the unique values in the phase variable will be used.
#' @param phase_labels (optional) A vector of labels corresponding to the phase_levels.
#'        If NULL, phase_levels will be used as labels.
#' @param covs (optional) A vector of names of covariates to be added to the
#'        fixed effect model.
#' @param ... Additional arguments passed to the `gls` function.
#'
#' @return A list containing three elements:
#'         - `data`: The modified dataset with added time variables and predicted
#'            values.
#'         - `fitted_mod`: The fitted model object from `nlme::gls`.
#'         - `plot`: A ggplot object showing the predicted values and phase
#'            annotations.
#'
#' @references Maric, M., & van der Werff, V. (2020). Single-Case Experimental
#' Designs in Clinical Intervention Research. In R. van de Schoot & M. Miocevic
#' (Eds.), Small Sample Size Solutions: A Guide for Applied Researchers and
#' Practitioners (1st ed., pp. 10). Routledge.
#' <doi:10.4324/9780429273872-9>
#'
#' @examples
#' res <- mixed_model_analysis(efficacy_of_CBT, .dv = "Anxious", .time = "time",
#'                             .phase = "phase",rev_time_in_phase = TRUE,
#'                             phase_levels = c(0, 1),
#'                             phase_labels = c("Exposure", "Exposure + CT"))
#'
#' summary(res$fitted_mod)
#'
#' @export
mixed_model_analysis <- function(.df, .dv, .time, .phase, .participant = NULL,
                                 rev_time_in_phase = FALSE, phase_levels = NULL,
                                 phase_labels = NULL, covs = NULL, ...) {

  # Initialize local variables
  time <- NULL
  .data <- NULL
  x <- NULL
  y <- NULL
  label <- NULL

  # Initial parameter checks
  if(!is.data.frame(.df)) stop(".df must be a data frame")
  if(!(.dv %in% names(.df))) stop(".dv must be a variable in .df")
  if(!(.time %in% names(.df))) stop(".time must be a variable in .df")
  if(!(.phase %in% names(.df))) stop(".phase must be a variable in .df")
  if(!(is.logical(rev_time_in_phase) & length(rev_time_in_phase) == 1))stop("rev_time_in_phase must be boolean")

  # Set factor levels and labels of the phase variable
  .df <- process_phase(.df, .phase, phase_levels, phase_labels)

  # Sort the data frame by phase and then by time
  .df <- .df[order(.df[[.phase]], .df[[.time]]), ]

  # Add a global time variable
  .df$time <- 1:nrow(.df)

  # Split the data frame by phase
  split_df <- split(.df, .df[[.phase]])

  # Use lapply to iterate over each phase and calculate time_in_phase
  split_df <- lapply(split_df, function(sub_df) {

    # Allow user to choose between countdown and regular count
    if(rev_time_in_phase){ # countdown
      sub_df$time_in_phase <- rev(seq_len(nrow(sub_df))) - 1
    } else if (!rev_time_in_phase){ # regular count
      sub_df$time_in_phase <- seq_len(nrow(sub_df)) - 1
    } else {
      stop("rev_time_in_phase must be TRUE or FALSE")
    }
    return(sub_df)
  })

  # Combine the list of data frames back into a single data frame
  .df <- do.call(rbind, split_df)

  # Set up fixed effect independent variable portion of model formula
  mod_form <- paste0("time_in_phase + ",.phase, " + time_in_phase*",.phase)

  # Add fixed effect covariates to the base model
  if (!is.null(covs)) {
    # Parameter check covs
    if(!all(covs %in% names(.df))) stop(paste0("covs contains variables that are not found in .df"))

    mod_form <- paste(mod_form, "+", covs)
  }

  # Add dependent variable
  mod_form <- stats::as.formula(paste0(.dv, " ~ ", mod_form))

  # Set up random effect portion of model formula
  if (is.null(.participant)){
    .df$participant <- factor(rep("x",nrow(.df)))
    re_form <- stats::as.formula("~time|participant")
  } else {
    re_form <- stats::as.formula(paste0("~time|",.participant))
  }

  # Fit model
  mod <- nlme::gls(model       = mod_form,
                   data        = .df,
                   method      = "REML",
                   correlation = nlme::corAR1(form=re_form),
                   ...)

  # Get predicted values
  .df[[paste0("predicted_", .dv)]] <- stats::predict(mod)

  # Identify where phase changes occur and calculate midpoints
  phase_changes <- which(diff(as.numeric(factor(.df[[.phase]]))) != 0)
  midpoints <- .df$time[phase_changes] + .5
  start_points <- c(1, midpoints + 0.5)
  end_points <- c(midpoints - 0.5, max(.df$time))

  # Prepare a data frame for annotations
  annotations_df <- data.frame(label = levels(factor(.df[[.phase]])),
                               x = (start_points + end_points) / 2,
                               y = rep(max(.df[[.dv]]) * 1.05, length(start_points))) # Adjust Y position as needed

  # Plot predicted values
  ggp <- ggplot2::ggplot()+
    ggplot2::geom_line(data= .df, ggplot2::aes(x =time,
                                               y = .data[[paste0("predicted_", .dv)]],
                                               col = "red"))

  # Add data points to plot (conditionally use shapes to distinguish participants)
  if (is.null(.participant)){
    ggp <- ggp + ggplot2::geom_point(data= .df, ggplot2::aes(x =time,
                                                             y= .data[[.dv]],
                                                             col = "blue"),
                                     size= 2)
  } else {
    ggp <- ggp + ggplot2::geom_point(data= .df, ggplot2::aes(x =time,
                                                             y= .data[[.dv]],
                                                             col = "blue",
                                                             shape = factor(.data[[.participant]])),
                                     size= 2)

  }

  # Add phases and styling to plot
  ggp <- ggp +
    ggplot2::geom_vline(xintercept = midpoints,
                        linetype = "dashed")+
    ggplot2::geom_text(data = annotations_df, ggplot2::aes(x = x, y = y, label = label), inherit.aes = FALSE) +
    ggplot2::theme_minimal()+
    ggplot2::ylab(.dv)+
    ggplot2::xlab("time")+
    ggplot2::scale_colour_manual(values=c("red"  = "red", "blue" = "blue"),
                                 labels= c("data", "model"))+
    ggplot2::theme(axis.ticks.x=ggplot2::element_blank(),
                   legend.title = ggplot2::element_blank())

  # Return results
  return(list(data = .df,
              fitted_mod = mod,
              plot = ggp))

}
