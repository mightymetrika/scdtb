#' Process Condition Variable
#'
#' This is an internal helper function used by `raw_plot` and `randomization_test`
#' to process the condition variable in the input data frame. It sets the factor
#' levels and labels of the condition variable based on the provided `cond_levels`
#' and `cond_labels` parameters.
#'
#' @param .df A data frame containing the data to be processed. Must contain a column
#'            specified by `.cond`.
#' @param .cond The name of the column in `.df` that contains the condition variable.
#' @param cond_levels (Optional) A vector of values indicating the order of condition
#'                    levels. This is used to set the factor levels of the condition
#'                    variable.
#' @param cond_labels (Optional) A vector of labels corresponding to the condition
#'                    levels. These are used for the legend. If not provided, the
#'                    `cond_levels` are used as labels.
#'
#' @return The input data frame `.df` with the condition variable processed as a factor
#'         with the specified levels and labels.
#'
#' @keywords internal
process_cond <- function(.df, .cond, cond_levels, cond_labels){
  # Set factor levels and labels of the condition variable
  if(!is.null(cond_levels)){

    # Initial parameter checks
    if(!is.character(.cond))stop(".cond must be an object of type character")
    if(!(.cond %in% names(.df))) stop(".cond must be a variable in .df")

    # Check cond_levels
    unique_cond_vals <- unique(.df[[.cond]])
    if(!all(cond_levels %in% unique_cond_vals)) stop(paste0("cond_levels contains values that are not found in: ", .cond))

    # Check dimension of labels agains levels
    if(!is.null(cond_labels)){
      if(length(cond_levels) != length(cond_labels)){
        stop("cond_levels and cond_labels must have the same length")
      }

      cond_labs <- cond_labels # Set labels
    } else {
      cond_labs <- cond_levels # Set labels
    }

    # Explicitly define phase variable as a factor with specified levels and labels
    .df[[.cond]] <- factor(.df[[.cond]], levels = cond_levels, labels = cond_labs)
  }

  # Make sure .cond is a factor
  if (!is.factor(.df[[.cond]])){
    .df[[.cond]] <- as.factor(.df[[.cond]])
  }

  return(.df)
}

#' Process Phase Variable
#'
#' This is an internal helper function used by `raw_plot` and `mixed_model_analysis`
#' to process the phase variable in the input data frame. It sets the factor levels
#' and labels of the phase variable based on the provided `phase_levels` and
#' `phase_labels` parameters.
#'
#' @param .df A data frame containing the data to be processed. Must contain a column
#'            specified by `.phase`.
#' @param .phase The name of the column in `.df` that contains the phase variable.
#' @param phase_levels (Optional) A vector of values indicating the order of phase
#'                     levels. This is used to set the factor levels of the phase
#'                     variable.
#' @param phase_labels (Optional) A vector of labels corresponding to the phase
#'                     levels. These labels are used in annotations. If not provided,
#'                     the `phase_levels` are used as labels.
#'
#' @return The input data frame `.df` with the phase variable processed as a factor
#'         with the specified levels and labels.
#'
#' @keywords internal
process_phase <- function(.df, .phase, phase_levels, phase_labels){
  # Set factor levels and labels of the phase variable
  if(!is.null(phase_levels)){

    # Initial parameter checks
    if(!is.character(.phase))stop(".phase must be an object of type character")
    if(!(.phase %in% names(.df))) stop(".phase must be a variable in .df")

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

  # Make sure .cond is a factor
  if (!is.factor(.df[[.phase]])){
    .df[[.phase]] <- as.factor(.df[[.phase]])
  }

  return(.df)
}

