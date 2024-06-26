% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cross_lagged.R
\name{cross_lagged}
\alias{cross_lagged}
\title{Cross-Lagged Correlation}
\usage{
cross_lagged(
  .df,
  .x,
  .y,
  lag.max = 5,
  na.action = stats::na.fail,
  conf.level = 0.95,
  ...
)
}
\arguments{
\item{.df}{A dataframe containing the variables for analysis.}

\item{.x}{The name of the first variable (as a string) to be analyzed.}

\item{.y}{The name of the second variable (as a string) to be analyzed.}

\item{lag.max}{The maximum lag at which to calculate the cross-correlation or covariance.
Defaults to 5.}

\item{na.action}{A function to specify the action to be taken if NAs are found.
Defaults to \code{stats::na.fail}.}

\item{conf.level}{The confidence level for the confidence intervals.
Defaults to 0.95.}

\item{...}{Additional arguments to be passed to stats::ccf().}
}
\value{
An object of class "cross_lagged" containing the cross-correlation function,
upper and lower confidence intervals, the number of observations used, and
the names of the variables analyzed.
}
\description{
Computes cross-lagged correlations between two variables in a dataframe.
}
\details{
This function calculates the cross-lagged correlation between two variables
in a given dataframe up to a specified maximum lag. It returns an object
containing the cross-correlation function, confidence intervals, and other
related information. The function calls stats::ccf() internally.
}
\examples{
# Creating a sample dataset
reversal_withdrawal <- data.frame(
  phase = c(rep("baseline1", 6), rep("treatment1", 5), rep("baseline2", 5), rep("treatment2", 5)),
  time = 1:21,
  extbehavs = c(15, 10, 14, 17, 13, 12, 2, 1, 1, 0, 0, 9, 9, 11, 15, 20, 1, 0, 4, 0, 1)
)

reversal_withdrawal$synth <- sapply(reversal_withdrawal$time, function(x) {
  stats::rpois(1, x)
})

reversal_withdrawal <- as.data.frame(reversal_withdrawal)

# Using the cross_lagged function
cl_result <- cross_lagged(reversal_withdrawal, .x = "time", .y = "synth")

}
