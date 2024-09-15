test_that("replext_mma works", {

  # Covariate specifications
  covariate_specs <- list(
    cov1 = list(dist = rnorm, args = list(mean = 0, sd = 1)),
    cov2 = list(dist = runif, args = list(min = 0, max = 1))
  )

  # Model formula
  formula <- y ~ phase * time_in_phase + cov1 + cov2

  # Regression coefficients
  betas <- c("(Intercept)" = 0,
             "phase1" = 0,
             "time_in_phase" = 0,
             "phase1:time_in_phase" = 0,
             "cov1" = 0,
             "cov2" = 0)

  # Time points per phase and autocorrelation coefficients
  n_timepoints_list <- c(5, 10)
  rho_list <- c(0.1, 0.3)

  # Number of iterations
  iterations <- 10

  results <- repelxt_mma(n_timepoints_list = n_timepoints_list,
                         rho_list = rho_list,
                         iterations = iterations,
                         n_phases = 2,
                         n_IDs = 1,
                         betas = betas,
                         formula = formula,
                         covariate_specs = covariate_specs)

  expect_equal(length(results), 4)
})

# n_timepoints_per_phase <- 5
# rho <- 0.1
# n_phases <- 2
# n_IDs = 1
# formula <- y ~ phase * time_in_phase
# # covariate_specs = NULL
# covariate_specs <- list(
#   cov1 = list(dist = rnorm, args = list(mean = 0, sd = 1)),
#   cov2 = list(dist = runif, args = list(min = 0, max = 1))
# )
# betas <- c("(Intercept)" = 0,
#            "phase1" = 0,
#            "time_in_phase" = 0,
#            "phase1:time_in_phase" = 0)

