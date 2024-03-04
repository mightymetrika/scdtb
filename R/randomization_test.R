randomization_test <- function(.df, .out, .cond, .time, num_permutations = NULL,
                               consec = c("observed", "fixed"), max_consec = NULL, min_consec = NULL,
                               cond_levels = NULL, cond_labels = NULL,
                               conf.level = 0.95, .bins = 30) {

  # Set local variables
  .data <- NULL
  x <- NULL

  # Get arguments from choices
  consec <- match.arg(consec)

  # Set factor levels and labels of the condition variable
  if(!is.null(cond_levels)){

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

  # Get unique factor levels
  cond_vec <- levels(.df[[.cond]])

  # Get test statistic
  original_diff <- mean(.df[[.out]][.df[[.cond]] == cond_vec[[2]]]) - mean(.df[[.out]][.df[[.cond]] == cond_vec[[1]]])

  # If num_permutations is NULL then get data frame of permissible sequences
  if(is.null(num_permutations)){

    # Get sequence length and numebr of measurements at first factor level
    seq_length <- length(.df[[.cond]])
    num_level_one <- sum(.df[[.cond]] == cond_vec[[1]])

    # Get full grid of sequences
    full_list <- rep(list(cond_vec), seq_length)
    full_grid <- expand.grid(full_list)

    # Filter down to grid with correct number of measurements at first factor level
    filtered_grid <- full_grid[apply(full_grid == cond_vec[[1]], 1, sum) == num_level_one, ]

    # Optionally filter down to maximum consecutive run length for any factor level
    if(!is.null(max_consec)){
      if(consec == "observed"){
        max_consecutive <- max(rle(as.character(.df[[.cond]]))$lengths)
      } else {
        if(!is.numeric(max_consec)) stop("max_consec must be an integer if num_permutations is not NULL and consec is set to fixed")
        max_consecutive <- max_consec
      }

      filtered_grid <- filtered_grid[apply(filtered_grid, 1, function(row) {
        max(rle(row)$lengths)
      }) <= max_consecutive, ]
    }

    # Optionally filter down to minimum consecutive run length for any factor level
    if(!is.null(min_consec)){
      if(consec == "observed"){
        min_consecutive <- min(rle(as.character(.df[[.cond]]))$lengths)
      } else {
        if(!is.numeric(min_consec)) stop("min_consec must be an integer if num_permutations is not NULL and consec is set to fixed")
        min_consecutive <- min_consec
      }

      filtered_grid <- filtered_grid[apply(filtered_grid, 1, function(row) {
        min(rle(as.character(row))$lengths)
      }) >= min_consecutive, ]
    }


  }

  # Get null distribution test statistics
  if (!is.null(num_permutations)){ # Use num_permutations to obtain distribution
    test_statistic_distribution <- replicate(num_permutations, {
      permuted_treatment <- sample(.df[[.cond]])
      # Ensure permuted_treatment maintains the factor structure of .df[[.cond]]
      permuted_treatment <- factor(permuted_treatment, levels = levels(.df[[.cond]]))
      # Calculate permuted difference
      permuted_diff <- mean(.df[[.out]][permuted_treatment == cond_vec[[2]]]) - mean(.df[[.out]][permuted_treatment == cond_vec[[1]]])
      permuted_diff
    })
  } else {
    test_statistic_distribution <- lapply(1:nrow(filtered_grid), function(row){
      permuted_treatment <- as.factor(as.vector(t(filtered_grid[row, ])))
      # Ensure permuted_treatment maintains the factor structure of .df[[.cond]]
      levels(permuted_treatment) <- levels(.df[[.cond]])
      # Calculate permuted difference
      permuted_diff <- mean(.df[[.out]][permuted_treatment == cond_vec[[2]]]) - mean(.df[[.out]][permuted_treatment == cond_vec[[1]]])
      permuted_diff
    }) |> unlist()
  }

  # Obtain p_value
  p_value <- mean(abs(test_statistic_distribution) >= abs(original_diff))

  # Calculate 95% Confidence Intervals
  c.lev <- (1 - conf.level)/2
  conf_int <- stats::quantile(test_statistic_distribution, probs = c(c.lev, 1 - c.lev))

  # Get distribution plot
  dist_plot <- ggplot2::ggplot(data.frame(x = test_statistic_distribution), ggplot2::aes(x = x)) +
    ggplot2::geom_histogram(bins = .bins) + # Adjusted for better visualization
    ggplot2::geom_vline(xintercept = conf_int[1], linetype = "dotted", color = "red", size = 1) + # Lower bound
    ggplot2::geom_vline(xintercept = conf_int[2], linetype = "dotted", color = "red", size = 1) + # Upper bound
    ggplot2::geom_vline(xintercept = original_diff, linetype = "dotted", color = "blue", size = 1) + # Original diff
    ggplot2::theme_minimal() +
    ggplot2::xlab("Mean Difference") +
    ggplot2::ggtitle("Distribution of Mean Differences with 95% CI and Original Difference")

  # Get raw data plot
  raw_plot <- ggplot2::ggplot(.df, ggplot2::aes(x = .data[[.time]], y = .data[[.out]], color = .data[[.cond]])) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::theme_minimal()


  # Return results
  return(list(original_diff = original_diff, p_value = p_value,
              test_statistic_distribution = test_statistic_distribution,
              conf_int = conf_int, dist_plot = dist_plot, raw_plot = raw_plot))
}
