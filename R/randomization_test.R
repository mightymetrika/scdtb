#' Randomization Test for Single-Case Experiments
#'
#' Performs a randomization test on data from single-case experiments. This
#' function allows for the assessment of treatment effects by comparing the observed
#' outcome difference to a distribution of differences obtained through permutation.
#' It supports various constraints on permutation sequences, such as fixed or
#' observed maximum and minimum consecutive sequences of a particular condition.
#' The function also provides graphical summaries of the raw data and the
#' distribution of mean differences.
#'
#' @param .df A data frame containing the variables of interest.
#' @param .out The name of the outcome variable in `.df`.
#' @param .cond The name of the condition variable in `.df`. This variable should
#'         have two levels.
#' @param .time The name of the time variable in `.df`.
#' @param num_permutations The number of permutations to perform. If `NULL`, all
#'        possible permutations are considered.
#' @param consec Specifies the constraint on consecutive sequences for permutation.
#'        Can be `"observed"` for the observed sequence length or `"fixed"` for
#'        a specified sequence length. Defaults to `"observed"`.
#' @param max_consec The maximum number of consecutive observations of the same
#'        condition to allow in permutations. If `NULL`, no maximum is enforced.
#'        To implement `max_consec`, `consec` must be set to `"fixed"`.
#' @param min_consec The minimum number of consecutive observations of the same
#'        condition to allow in permutations. If `NULL`, no minimum is enforced.
#'        To implement `min_consec`, `consec` must be set to `"fixed"`.
#' @param cond_levels Explicitly sets the levels of the condition variable. If
#'        `NULL`, the levels are derived from the data.
#' @param cond_labels Labels for the condition levels. If `NULL`, levels are used
#'        as labels.
#' @param conf.level The confidence level for the confidence interval calculation.
#'        Defaults to 0.95.
#' @param .bins The number of bins to use for the histogram of the test statistic
#'        distribution. Defaults to 30.
#'
#' @return A list containing the original difference in means, the p-value of the
#'         test, the distribution of test statistics under the null hypothesis,
#'         confidence intervals, and plots of the distribution of mean differences
#'         and the raw data.
#'
#' @examples
#' result <- randomization_test(sleeping_pills, .out = "sever_compl",
#'                              .cond = "treatment", .time = "day",
#'                              num_permutations = 100,
#'                              cond_levels = c("C", "E"))
#'
#' result$conf_int
#'
#' @references
#' Onghena, P. (2020). One by One: The design and analysis of replicated randomized
#' single-case experiments.In R. van de Schoot & M. Mioecvic (Eds.), Small Sample
#' Size Solutions: A Guide for Applied Researchers and Practitioners (1st ed., pp.
#' 15). Routledge. <doi:10.4324/9780429273872-8>
#'
#' @export
randomization_test <- function(.df, .out, .cond, .time, num_permutations = NULL,
                               consec = c("observed", "fixed"), max_consec = NULL, min_consec = NULL,
                               cond_levels = NULL, cond_labels = NULL,
                               conf.level = 0.95, .bins = 30) {

  # Set local variables
  .data <- NULL
  x <- NULL

  # Get arguments from choices
  consec <- match.arg(consec)

  # Initial parameter checks
  if(!is.data.frame(.df)) stop(".df must be a data frame")
  if(!(.out %in% names(.df))) stop(".out must be a variable in .df")
  if(!(.cond %in% names(.df))) stop(".cond must be a variable in .df")
  if(!(.time %in% names(.df))) stop(".time must be a variable in .df")
  if(.bins %% 1 != 0) stop(".bins must be a whole number")
  if(!(conf.level >= 0 & conf.level <= 1)) stop("conf.level must be between 0 and 1")

  # Set factor levels and labels of the condition variable
  .df <- process_cond(.df, .cond, cond_levels, cond_labels)

  # Get unique factor levels
  cond_vec <- levels(.df[[.cond]])
  if(length(cond_vec) != 2) stop("function designed to test two factor levels")

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
  conf_int <- stats::quantile(test_statistic_distribution,
                              probs = c((1 - conf.level)/2, 1 - (1 - conf.level)/2))

  # Get distribution plot
  dist_plot <- ggplot2::ggplot(data.frame(x = test_statistic_distribution), ggplot2::aes(x = x)) +
    ggplot2::geom_histogram(bins = .bins) + # Adjusted for better visualization
    ggplot2::geom_vline(xintercept = conf_int[1], linetype = "dotted", color = "red") + # Lower bound
    ggplot2::geom_vline(xintercept = conf_int[2], linetype = "dotted", color = "red") + # Upper bound
    ggplot2::geom_vline(xintercept = original_diff, linetype = "dotted", color = "blue") + # Original diff
    ggplot2::theme_minimal() +
    ggplot2::xlab("Mean Difference") +
    ggplot2::ggtitle("Distribution of Mean Differences with 95% CI and Original Difference")

  # Get raw data plot
  raw_plot <- ggplot2::ggplot(.df, ggplot2::aes(x = .data[[.time]], y = .data[[.out]], color = .data[[.cond]])) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::theme_minimal() +
    ggplot2::ggtitle("Raw Data by Condition")

  # Return results
  return(list(original_diff = original_diff, p_value = p_value,
              test_statistic_distribution = test_statistic_distribution,
              conf_int = conf_int, dist_plot = dist_plot, raw_plot = raw_plot))
}
