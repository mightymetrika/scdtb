cross_lagged <- function(.df, .x, .y, lag.max = NULL, type = c("correlation", "covariance"),
                         plot = TRUE, na.action = stats::na.fail, ...){

  stats::ccf(x = .df[[.x]], y = .df[[.y]], lag.max, type, plot, na.action, ...)

}
