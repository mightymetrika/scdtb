#' Cross-Lagged Correlation
#'
#' Computes cross-lagged correlations between two variables in a dataframe.
#'
#' This function calculates the cross-lagged correlation between two variables
#' in a given dataframe up to a specified maximum lag. It returns an object
#' containing the cross-correlation function, confidence intervals, and other
#' related information. The function calls stats::ccf() internally.
#'
#' @param .df A dataframe containing the variables for analysis.
#' @param .x The name of the first variable (as a string) to be analyzed.
#' @param .y The name of the second variable (as a string) to be analyzed.
#' @param lag.max The maximum lag at which to calculate the cross-correlation or covariance.
#'        Defaults to 5.
#' @param na.action A function to specify the action to be taken if NAs are found.
#'        Defaults to `stats::na.fail`.
#' @param conf.level The confidence level for the confidence intervals.
#'        Defaults to 0.95.
#' @param ... Additional arguments to be passed to stats::ccf().
#'
#' @return An object of class "cross_lagged" containing the cross-correlation function,
#'         upper and lower confidence intervals, the number of observations used, and
#'         the names of the variables analyzed.
#'
#' @examples
#' # Creating a sample dataset
#' reversal_withdrawal <- data.frame(
#'   phase = c(rep("baseline1", 6), rep("treatment1", 5), rep("baseline2", 5), rep("treatment2", 5)),
#'   time = 1:21,
#'   extbehavs = c(15, 10, 14, 17, 13, 12, 2, 1, 1, 0, 0, 9, 9, 11, 15, 20, 1, 0, 4, 0, 1)
#' )
#'
#' reversal_withdrawal$synth <- sapply(reversal_withdrawal$time, function(x) {
#'   stats::rpois(1, x)
#' })
#'
#' reversal_withdrawal <- as.data.frame(reversal_withdrawal)
#'
#' # Using the cross_lagged function
#' cl_result <- cross_lagged(reversal_withdrawal, .x = "time", .y = "synth")
#'
#' @export
cross_lagged <- function(.df, .x, .y, lag.max = 5, na.action = stats::na.fail,
                         conf.level = 0.95, ...){

  if(!is.data.frame(.df)) stop(".df must be a data frame")
  if(!(.x %in% names(.df))) stop(".x must be a variable in .df")
  if(!(.y %in% names(.df))) stop(".y must be a variable in .df")
  if(!(conf.level >= 0 & conf.level <= 1)) stop("conf.level must be between 0 and 1")

  # Get cross lagged correlation or covariance
  cl <- stats::ccf(x = .df[[.x]], y = .df[[.y]], lag.max, type = "correlation",
                   plot = FALSE, na.action, ...)

  # Get confidence intervals
  cl$UCI <- stats::qnorm((1+conf.level)/2)/sqrt(cl$n.used)
  cl$LCI <- -stats::qnorm((1+conf.level)/2)/sqrt(cl$n.used)

  # Set snames attribute
  cl$snames <- paste0(.x, " & ", .y)

  # Add class
  class(cl) <- "cross_lagged"

  # Return object
  return(cl)

}

#' Plot Cross-Lagged Correlation Results
#'
#' Plots the cross-lagged correlation results calculated by cross_lagged().
#' This method generates a bar plot showing the autocorrelation function (ACF) values
#' for different lags, along with dashed lines indicating the upper and lower confidence
#' intervals. The plot is created using `ggplot2`.
#'
#' @param x An object of class "cross_lagged" containing the results of a cross-lagged
#'        correlation analysis performed by cross_lagged().
#' @param ... Additional parameters (currently ignored).
#'
#' @return A `ggplot` object representing the cross-lagged correlation analysis
#'        results. This plot includes bars for ACF values at different lags
#'        and dashed lines for the upper and lower confidence intervals.
#'
#' @examples
#' #Creating a sample dataset
#' reversal_withdrawal <- data.frame(
#'   phase = c(rep("baseline1", 6), rep("teratment1", 5), rep("baseline2", 5), rep("teratment2", 5)),
#'   time = 1:21,
#'   extbehavs = c(15, 10, 14, 17, 13, 12, 2, 1, 1, 0, 0, 9, 9, 11, 15, 20, 1, 0, 4, 0, 1)
#' )
#'
#' reversal_withdrawal$synth <- sapply(reversal_withdrawal$time, function(x) {
#'   stats::rpois(1, x)
#' })
#'
#' reversal_withdrawal <- as.data.frame(reversal_withdrawal)
#'
#' # Using the cross_lagged function
#' cl_result <- cross_lagged(reversal_withdrawal, .x = "time", .y = "synth")
#'
#' # Plot the cross-lagged correlation results
#' plot(cl_result)
#'
#' @export
plot.cross_lagged <- function(x, ...) {

  # Parameter checks
  if(!inherits(x, "cross_lagged"))stop("x must be an object of class cross_lagged")

  # Set local variables
  lag <- NULL
  acf <- NULL

  # Convert to data frame for plotting
  data <- data.frame(lag = x$lag[,,1], acf = x$acf[,,1])

  # Make plot title
  plot_title <- paste0("Cross Lagged Correlation: ", x$snames)

  # Create plot object
  ggplot2::ggplot(data, ggplot2::aes(x = lag, y = acf)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::geom_hline(yintercept = x$UCI, linetype = "dashed", color = "red") +
    ggplot2::geom_hline(yintercept = x$LCI, linetype = "dashed", color = "red") +
    ggplot2::labs(title = plot_title, x = "Lag", y = "ACF") +
    ggplot2::theme_minimal()
}

#' Print Method for Cross-Lagged Objects
#'
#' Prints a summary of cross-lagged correlation analysis results. This method
#' formats the results into a data frame showing lags, autocorrelation function (ACF) values,
#' and indicates significance based on the confidence intervals. Significant ACF values,
#' which fall outside the upper or lower confidence intervals, are marked with an asterisk (*).
#'
#' @param x An object of class "cross_lagged" containing the results from running
#'        `cross_lagged`. This object includes information on lags, ACF values, and
#'        confidence intervals.
#' @param ... Additional parameters (currently ignored).
#'
#' @return Invisibly returns a data frame with columns for lag, ACF values, and a
#'         significance indicator. This data frame is printed to the console for the user.
#'
#' @examples
#' #Creating a sample dataset
#' reversal_withdrawal <- data.frame(
#'   phase = c(rep("baseline1", 6), rep("teratment1", 5), rep("baseline2", 5), rep("teratment2", 5)),
#'   time = 1:21,
#'   extbehavs = c(15, 10, 14, 17, 13, 12, 2, 1, 1, 0, 0, 9, 9, 11, 15, 20, 1, 0, 4, 0, 1)
#' )
#'
#' reversal_withdrawal$synth <- sapply(reversal_withdrawal$time, function(x) {
#'   stats::rpois(1, x)
#' })
#'
#' reversal_withdrawal <- as.data.frame(reversal_withdrawal)
#'
#' # Using the cross_lagged function
#' cl_result <- cross_lagged(reversal_withdrawal, .x = "time", .y = "synth")
#'
#' # Print the summary of cross-lagged analysis
#' print(cl_result)
#'
#' @export
print.cross_lagged <- function(x, ...) {

  # Parameter checks
  if(!inherits(x, "cross_lagged"))stop("x must be an object of class cross_lagged")

  # Create a data frame with lag and acf
  results <- data.frame(lag = x$lag[,,1], acf = x$acf[,,1])

  # Determine significance
  results$significant <- ifelse(results$acf > x$UCI | results$acf < x$LCI, "*", "")

  # Print the data frame
  print(results)

  invisible(results) # To return the results silently
}
