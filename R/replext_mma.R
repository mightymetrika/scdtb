# Modified simulate_mma_once function
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
  mod <- tryCatch({
    nlme::gls(model = formula,
              data = df_all,
              correlation = nlme::corAR1(form = ~ time | ID),
              method = "REML")
  }, error = function(e) {
    return(NULL)
  })

  if (is.null(mod)) {
    # Model failed to converge
    return(list(success = FALSE))
  }

  # Extract results
  summary_mod <- summary(mod)
  coefs <- summary_mod$tTable
  estimates <- coefs[, "Value"]
  std_errors <- coefs[, "Std.Error"]
  p_values <- coefs[, "p-value"]

  # Compute bias (estimated betas - true betas)
  bias <- estimates[names(betas)] - betas
  # Compute RMSE
  rmse <- sqrt(mean((estimates[names(betas)] - betas)^2))

  # Return results
  return(list(success = TRUE,
              estimates = estimates,
              std_errors = std_errors,
              p_values = p_values,
              bias = bias,
              rmse = rmse))
}

# Modified replext_mma function
replext_mma <- function(n_timepoints_list, rho_list, iterations, n_phases = 2, n_IDs = 1,
                        betas, formula, covariate_specs = NULL, verbose = FALSE) {

  results_list <- list()
  for (n_tp in n_timepoints_list) {
    for (rho in rho_list) {
      if(verbose){
        cat("Running simulation for n_timepoints_per_phase =", n_tp, "and rho =", rho, "\n")
      }
      # Initialize lists to store outputs
      success_list <- vector("logical", iterations)
      estimates_list <- vector("list", iterations)
      std_errors_list <- vector("list", iterations)
      p_values_list <- vector("list", iterations)
      bias_list <- vector("list", iterations)
      rmse_list <- numeric(iterations)

      for (i in 1:iterations) {
        res <- simulate_mma_once(n_timepoints_per_phase = n_tp,
                                 rho = rho,
                                 n_phases = n_phases,
                                 n_IDs = n_IDs,
                                 betas = betas,
                                 formula = formula,
                                 covariate_specs = covariate_specs)
        success_list[i] <- res$success
        if (res$success) {
          estimates_list[[i]] <- res$estimates
          std_errors_list[[i]] <- res$std_errors
          p_values_list[[i]] <- res$p_values
          bias_list[[i]] <- res$bias
          rmse_list[i] <- res$rmse
        } else {
          estimates_list[[i]] <- NA
          std_errors_list[[i]] <- NA
          p_values_list[[i]] <- NA
          bias_list[[i]] <- NA
          rmse_list[i] <- NA
        }
      }
      # Compute success rate
      success_rate <- mean(success_list)
      # Compute mean estimates, biases, rmse over successful iterations
      estimates_matrix <- do.call(rbind, estimates_list[success_list])
      std_errors_matrix <- do.call(rbind, std_errors_list[success_list])
      p_values_matrix <- do.call(rbind, p_values_list[success_list])
      bias_matrix <- do.call(rbind, bias_list[success_list])
      rmse_vector <- rmse_list[success_list]

      mean_estimates <- colMeans(estimates_matrix, na.rm = TRUE)
      mean_bias <- colMeans(bias_matrix, na.rm = TRUE)
      mean_rmse <- mean(rmse_vector, na.rm = TRUE)

      # Compute standard errors of estimates
      se_estimates <- apply(estimates_matrix, 2, stats::sd, na.rm = TRUE) / sqrt(sum(success_list))
      # Compute rejection rates
      rejection_rates <- colMeans(p_values_matrix < 0.05, na.rm = TRUE)
      # Compute standard error of rejection rates
      se_rejection_rates <- sqrt(rejection_rates * (1 - rejection_rates) / sum(success_list))

      # Store results
      results_list[[paste("n_tp", n_tp, "rho", rho, sep = "_")]] <- list(
        success_rate = success_rate,
        mean_estimates = mean_estimates,
        mean_bias = mean_bias,
        mean_rmse = mean_rmse,
        se_estimates = se_estimates,
        rejection_rates = rejection_rates,
        se_rejection_rates = se_rejection_rates
      )
    }
  }
  return(results_list)
}
