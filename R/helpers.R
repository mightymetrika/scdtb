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

