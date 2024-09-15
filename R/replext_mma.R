simulate_mma_once <- function(n_timepoints_per_phase, rho, n_phases = 2, n_IDs = 1,
                              betas, formula, covariate_specs = NULL) {
  # set up data points and phases
  n_total <- n_timepoints_per_phase * n_phases
  phase_levels <- 0:(n_phases - 1)
  phase <- rep(phase_levels, each = n_timepoints_per_phase)

  # add phases and timepoints to dataframe
  df <- data.frame(phase = factor(phase))
  time_in_phase <- rep(0:(n_timepoints_per_phase - 1), times = n_phases)
  df$time_in_phase <- time_in_phase

  # add covariates to dataframe
  if (!is.null(covariate_specs)) {
    for (cov_name in names(covariate_specs)) {
      spec <- covariate_specs[[cov_name]]
      if (!is.null(spec$expr)) {
        df[[cov_name]] <- spec$expr(n_total, df)
      } else {
        dist_fun <- spec$dist
        dist_args <- spec$args
        df[[cov_name]] <- do.call(dist_fun, c(list(n = n_total), dist_args))
      }
    }
  }

  # Corrected line: exclude the response variable
  mm_sample <- stats::model.matrix(stats::delete.response(stats::terms(formula)), data = df)
  beta_names <- colnames(mm_sample)

  if (!all(beta_names %in% names(betas))) {
    stop("The names of betas must match the column names in the model matrix.")
  }

  df_list <- list()
  for (id in 1:n_IDs) {
    df_id <- df
    df_id$ID <- factor(id)
    # Corrected line: exclude the response variable
    mm <- stats::model.matrix(stats::delete.response(stats::terms(formula)), data = df_id)
    lp <- as.vector(mm %*% betas[beta_names])
    e <- as.numeric(stats::arima.sim(n = n_total, model = list(ar = rho)))
    y <- lp + e
    df_id$y <- y
    df_list[[id]] <- df_id
  }

  df_all <- do.call(rbind, df_list)
  df_all$time <- rep(1:n_total, times = n_IDs)

  # fit model
  mod <- nlme::gls(model = formula,
                   data = df_all,
                   correlation = nlme::corAR1(form = ~ time | ID),
                   method = "REML")

  # extract results
  summary_mod <- summary(mod)
  coefs <- summary_mod$tTable
  p_values <- coefs[, "p-value"]

  # return results
  return(p_values)
}

# run_simulation function
repelxt_mma <- function(n_timepoints_list, rho_list, iterations, n_phases = 2, n_IDs = 1,
                           betas, formula, covariate_specs = NULL, verbose = FALSE) {


  results_list <- list()
  for (n_tp in n_timepoints_list) {
    for (rho in rho_list) {
      if(verbose){
        cat("Running simulation for n_timepoints_per_phase =", n_tp, "and rho =", rho, "\n")
      }
      p_values_matrix <- replicate(iterations, {
        p_values <- simulate_mma_once(n_timepoints_per_phase = n_tp,
                                      rho = rho,
                                      n_phases = n_phases,
                                      n_IDs = n_IDs,
                                      betas = betas,
                                      formula = formula,
                                      covariate_specs = covariate_specs)
        return(p_values)
      })
      type1_errors <- rowMeans(p_values_matrix < 0.05)
      results_list[[paste("n_tp", n_tp, "rho", rho, sep = "_")]] <- type1_errors
    }
  }
  return(results_list)
}
