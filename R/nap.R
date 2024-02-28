nap <- function(.df, .y, .phase, .time, type = c("reversability", "trend"), last_m = NULL,
                phases, improvement = c("positive", "negative")){

  # Sort the data frame by time within each phase
  .df <- .df[order(.df[[.phase]], .df[[.time]]), ]

  if (type == "reversability") {
    series <- lapply(unique(phases), function(x) {
      subsetted_df <- .df[.df[[.phase]] == x, ]
      return(subsetted_df[[.y]])
    })

  }

  if (type == "trend"){
      subsetted_df <- .df[.df[[.phase]] == phases, ]
      series_a <- subsetted_df[1:(nrow(subsetted_df) - last_m),]
      series_b <- subsetted_df[(nrow(subsetted_df) - last_m + 1):nrow(subsetted_df),]
      series <- list(series_a[[.y]], series_b[[.y]])
  }

  comps <- lapply(series[[1]], function(item1) {
    sapply(series[[2]], function(item2) {
      if(improvement == "positive"){
        ifelse(item1 < item2, 1, ifelse(item1 > item2, 0, 0.5))
      } else if (improvement == "negative"){
        ifelse(item1 > item2, 1, ifelse(item1 < item2, 0, 0.5))
      } else {
        stop("improvement must be set to 'positive' or 'negative'")
      }

    })
  }) |>
    unlist()

  result <- sum(comps)/(length(series[[1]])*length(series[[2]]))


}




# .df <- basic_scd
# .y <- "socbehavs"
# .phase <- "phase"
# phases <- list("baseline")
# last_m <- 3
