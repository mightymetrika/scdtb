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
