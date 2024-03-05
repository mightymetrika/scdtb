#' Non-overlap of All Pairs (NAP) Analysis
#'
#' This function performs a Non-overlap of All Pairs (NAP) analysis on a given
#' data frame, considering specified phases, improvement direction, and analysis
#' type (reversability or trend). It is designed to assess the distinctiveness of
#' data across phases or trends within the data, based on the concept outlined in
#' the What Works Clearinghouse Procedures and Standards Handbook.
#'
#' @param .df A data frame containing the data to be analyzed.
#' @param .y Character string specifying the variable in `.df` to be analyzed.
#' @param .phase Character string specifying the variable in `.df` that defines
#'        the phases.
#' @param .time Character string specifying the time variable in `.df`.
#' @param type Character string indicating the type of analysis to be conducted:
#'        either "reversability" or "trend".
#' @param last_m An integer specifying the number of measurements from the end
#'        to be considered in a trend analysis. Leave as NULL if `type` is set
#'        to "reversability".
#' @param phases Vector specifying the phases to be included in the analysis.
#'        If `type` is "reversability", two phases must be specified. If `type`
#'        is "trend", one phase must be specified.
#' @param improvement Character vector indicating the direction of improvement to
#'        consider: either "positive" or "negative".
#'
#' @return A numeric value representing the NAP score, reflecting the proportion
#'         of non-overlapping data points between the specified phases or trends.
#'
#' @details
#' The NAP analysis is a method used to evaluate the effectiveness of interventions
#' by analyzing the non-overlap between data points across different phases or
#' trends within a dataset. It is a useful statistical tool for educational research
#' and is detailed in the What Works Clearinghouse Procedures and Standards Handbook,
#' Version 5.0.
#'
#' @references
#' What Works Clearinghouse. (2022). What Works Clearinghouse procedures and
#' standards handbook, version 5.0. U.S. Department of Education, Institute of
#' Education Sciences, National Center for Education Evaluation and Regional
#' Assistance (NCEE). This report is available on the What Works Clearinghouse
#' website at \url{https://ies.ed.gov/ncee/wwc/Handbooks}.
#'
#' @examples
#' nap(.df = reversal_withdrawal, .y = "extbehavs", .phase = "phase",
#'     .time = "time", type = "reversability",
#'     phases = list("baseline1", "baseline2"), improvement = "negative")
#'
#' @export
nap <- function(.df, .y, .phase, .time, type = c("reversability", "trend"),
                last_m = NULL, phases, improvement = c("positive", "negative")){

  # Get argument from choices
  improvement <- match.arg(improvement)
  type <- match.arg(type)

  # Initial parameter checks
  if(!is.character(.y))stop(".y must be an object of type character")
  if(!is.character(.phase))stop(".phase must be an object of type character")
  if(!is.character(.time))stop(".time must be an object of type character")
  if(!is.vector(phases))stop("phases must be an object of type vector")
  if(!is.data.frame(.df)) stop(".df must be a data frame")
  if(!(.y %in% names(.df))) stop(".y must be a variable in .df")
  if(!(.phase %in% names(.df))) stop(".phase must be a variable in .df")
  if(!(.time %in% names(.df))) stop(".time must be a variable in .df")
  if(!is.null(last_m)){if(last_m %% 1 != 0) stop("last_m must be a whole number")}
  if(!all(phases %in% unique(.df[[.phase]]))) stop(paste0("phases contains values that are not found in: ", .phase))
  if(is.null(last_m)){if(length(phases) != 2)stop("length of phases must equal two if last_m is NULL")}
  if(!is.null(last_m)){if(length(phases) != 1)stop("length of phases must equal one if last_m is not NULL")}


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
      } else {
        ifelse(item1 > item2, 1, ifelse(item1 < item2, 0, 0.5))
      }

    })
  }) |>
    unlist()

  sum(comps)/(length(series[[1]])*length(series[[2]]))

}
