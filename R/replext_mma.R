simulate_mma_once <- function(n_timepoints_per_phase, rho, n_phases = 2, n_IDs = 1,
                              betas, formula, covariate_specs = NULL) {
  # Set up data points and phases
  n_total <- n_timepoints_per_phase * n_phases
  phase_levels <- 0:(n_phases - 1)
  phase <- rep(phase_levels, each = n_timepoints_per_phase)

  # Add phases and timepoints to dataframe
  df <- data.frame(phase = factor(phase))
  time_in_phase <- rep((n_timepoints_per_phase - 1):0, times = n_phases)
  df$time_in_phase <- time_in_phase

  # Add ID level data to dataframe
  df_list <- vector(mode = "list", length = n_IDs)
  for (id in 1:n_IDs) {
    df_id <- df
    df_id$ID <- factor(id)

    # Generate covariates for this ID
    if (!is.null(covariate_specs)) {
      for (spec in covariate_specs) {
        vars <- spec$vars
        dist <- spec$dist
        args <- spec$args
        n <- n_total
        if (!is.null(spec$expr)) {
          data_mat <- spec$expr(n, df_id)
          for (i in seq_along(vars)) {
            df_id[[vars[i]]] <- data_mat[, i]
          }
        } else if (length(vars) > 1) {
          # Multivariate variables
          if (dist == "mvnorm") {
            data_mat <- MASS::mvrnorm(n = n, mu = args$mu, Sigma = args$Sigma)
            for (i in seq_along(vars)) {
              df_id[[vars[i]]] <- data_mat[, i]
            }
          } else if (dist == "rmsn") {
            data_mat <- sn::rmsn(n = n, xi = args$xi, Omega = args$Omega, alpha = args$alpha)
            for (i in seq_along(vars)) {
              df_id[[vars[i]]] <- data_mat[, i]
              }
          } else {
            stop("Unsupported multivariate distribution")
          }
        } else {
          # Univariate variable
          data_vec <- do.call(dist, c(list(n = n), args))
          df_id[[vars]] <- data_vec
        }
      }
    }

    # Create model matrix and get beta names (excluding response variable)
    mm <- stats::model.matrix(stats::delete.response(stats::terms(formula)), data = df_id)
    beta_names <- colnames(mm)

    if (!all(beta_names %in% names(betas))) {
      stop("The names of betas must match the column names in the model matrix.")
    }

    # compute linear predictor
    lp <- as.vector(mm %*% betas[beta_names])
    # Generate AR(1) errors
    e <- as.numeric(stats::arima.sim(n = n_total, model = list(ar = rho)))
    # Compute response variable
    y <- lp + e
    df_id$y <- y
    df_list[[id]] <- df_id
  }

  df_all <- do.call(rbind, df_list)
  df_all$time <- rep(1:n_total, times = n_IDs)

  # Fit model
  mod <- nlme::gls(model = formula,
                   data = df_all,
                   correlation = nlme::corAR1(form = ~ time | ID),
                   method = "REML")

  # Extract results
  summary_mod <- summary(mod)
  coefs <- summary_mod$tTable
  p_values <- coefs[, "p-value"]

  # Return results
  return(p_values)
}


# run_simulation function
replext_mma <- function(n_timepoints_list, rho_list, iterations, n_phases = 2, n_IDs = 1,
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
