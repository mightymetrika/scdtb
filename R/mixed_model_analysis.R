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
  if(!is.null(phase_levels)){

    # Check phase levels
    if(!all(phase_levels %in% unique(.df[[.phase]]))) stop(paste0("phase_levels contains values that are not found in: ", .phase))

    # Check dimension of labels agains levels
    if(!is.null(phase_labels)){
      if(length(phase_levels) != length(phase_labels)){
        stop("phase_levels and phase_labels must have the same length")
      }

      ph_labs <- phase_labels # Set labels
    } else {
      ph_labs <- phase_levels # Set labels
    }

    # Explicitly define phase variable as a factor with specified levels and labels
    .df[[.phase]] <- factor(.df[[.phase]], levels = phase_levels, labels = ph_labs)
  }

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
                   correlation = nlme::corAR1(, form=re_form),
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

  # Plot data with predicted values
  ggp <- ggplot2::ggplot()+
    ggplot2::geom_line(data= .df, ggplot2::aes(x =time,
                                               y = .data[[paste0("predicted_", .dv)]],
                                               col = "red"),
                       size= 2)+
    ggplot2::geom_point(data= .df, ggplot2::aes(x =time,
                                                 y= .data[[.dv]],
                                                 col = "blue"),
                        size= 2)+
    ggplot2::geom_vline(xintercept = midpoints,
                        linetype = "dashed",
                        size= 1.2)+
    ggplot2::geom_text(data = annotations_df, ggplot2::aes(x = x, y = y, label = label), inherit.aes = FALSE) +
    ggplot2::theme_minimal()+
    ggplot2::ylab(.dv)+
    ggplot2::xlab("time")+
    ggplot2::scale_colour_manual(values=c("red"  = "red", "blue" = "blue"),
                                 labels= c("data", "model"))+
    ggplot2::theme(axis.ticks.x=ggplot2::element_blank(),
                   legend.title = ggplot2::element_blank())

  return(list(data = .df,
              fitted_mod = mod,
              plot = ggp))

}

