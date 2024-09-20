#' Generate UI Elements for GLS Simulation Parameters
#'
#' This internal function creates a list of Shiny UI input elements for setting
#' parameters in the Generalized Least Squares (GLS) simulation Shiny app.
#'
#' @return A list of Shiny input elements, including:
#'   \itemize{
#'     \item textInput for number of timepoints in each phase
#'     \item textInput for AR(1) parameter values
#'     \item numericInput for number of simulation iterations
#'     \item numericInput for number of phases
#'     \item numericInput for number of participants
#'     \item textAreaInput for regression coefficients
#'     \item textAreaInput for model formula
#'     \item textAreaInput for covariate specification
#'     \item numericInput for alpha level
#'   }
#'
#' @details
#' This function is used internally by the \code{replext_pgsql} Shiny app to
#' generate the UI elements for parameter input. It creates a standardized set
#' of inputs with default values that users can modify to customize their
#' simulation settings.
#'
#' @note
#' The function uses text inputs for lists (e.g., timepoints, AR(1) parameters)
#' which should be entered as comma-separated values. The betas, formula, and
#' covariate specifications are entered as text areas to allow for more complex
#' inputs.
#'
#' @seealso \code{\link{replext_pgsql}} for the main Shiny app function.
#'
#' @keywords internal
replext_gls_UIParams <- function() {
  list(
    shiny::textInput("n_timepoints_list", "Number of timepoints in each phase (list):", "5, 10"),
    shiny::textInput("rho_list", "AR(1) parameter (list)", "0.1, 0.3"),
    shiny::numericInput("iterations", "Number of simulation iterations", 100),
    shiny::numericInput("n_phases", "Number of phases", 2),
    shiny::numericInput("n_IDs", "Number of participants", 1),
    shiny::textAreaInput("betas", "Regression coefficients",
                         "'(Intercept)' = 0,
    'phase1' = 0,
    'time_in_phase' = 0,
    'phase1:time_in_phase' = 0"),
    shiny::textAreaInput("formula", "Model formula", "y ~ phase * time_in_phase"),
    shiny::textAreaInput("covariate_specs", "Covariate specification", "NULL"),
    shiny::numericInput("alpha", "Alpha level", 0.05)
  )
}
