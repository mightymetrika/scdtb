#' Simulate and Analyze Generalized Least Squares
#'
#' This function simulates data and fits a Generalized Least Squares (GLS) model
#' to the simulated data. It can handle multiple phases, multiple subjects (IDs),
#' and various covariate specifications.
#'
#' @param n_timepoints_per_phase Integer. The number of timepoints in each phase.
#' @param rho Numeric. The autocorrelation parameter for the AR(1) error structure.
#' @param n_phases Integer. The number of phases in the design. Default is 2.
#' @param n_IDs Integer. The number of subjects (IDs) to simulate. Default is 1.
#' @param betas Named numeric vector. The true coefficient values for the model.
#' @param formula Formula object. The model formula to be fitted.
#' @param covariate_specs List of lists. Specifications for generating covariates.
#'   Each inner list should contain elements 'vars' (variable names), 'dist'
#'   (distribution function name), args' (list of distribution parameters), and
#'   optionally 'expr' (a function to generate data).
#'
#' @return A list containing:
#'   \item{success}{Logical. TRUE if the model converged, FALSE otherwise.}
#'   \item{estimates}{Named numeric vector. Estimated coefficients from the fitted model.}
#'   \item{std_errors}{Named numeric vector. Standard errors of the estimated coefficients.}
#'   \item{p_values}{Named numeric vector. P-values for the estimated coefficients.}
#'   \item{bias}{Named numeric vector. Bias of the estimated coefficients (estimate - true value).}
#'   \item{rmse}{Numeric. Root Mean Square Error of the estimated coefficients.}
#'
#' @examples
#' # Simple example with 2 phases, 10 timepoints per phase, and no covariates
#' result <- simulate_gls_once(
#'   n_timepoints_per_phase = 10,
#'   rho = 0.2,
#'   betas = c("(Intercept)" = 0, "phase1" = 1),
#'   formula = y ~ phase
#' )
#'
#' @export
simulate_gls_once <- function(n_timepoints_per_phase, rho, n_phases = 2, n_IDs = 1,
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

#' Replications and Extension of Generalized Least Squares Simulation
#'
#' This function performs multiple simulations of Generalized Least Squares (GLS)
#' models across various conditions, extending the work of Maric et al. (2014).
#' It allows for the exploration of different numbers of timepoints per phase and
#' autocorrelation parameters, providing a comprehensive analysis of model
#' performance across these conditions.
#'
#' @param n_timepoints_list Numeric vector. The numbers of timepoints per phase
#'   to simulate.
#' @param rho_list Numeric vector. The autocorrelation parameters to simulate.
#' @param iterations Integer. The number of simulations to run for each combination
#'   of conditions.
#' @param n_phases Integer. The number of phases in the design. Default is 2.
#' @param n_IDs Integer. The number of subjects (IDs) to simulate. Default is 1.
#' @param betas Named numeric vector. The true coefficient values for the model.
#' @param formula Formula object. The model formula to be fitted.
#' @param covariate_specs List of lists. Specifications for generating covariates.
#'   Each inner list should contain elements 'vars' (variable names), 'dist'
#'   (distribution function name), 'args' (list of distribution parameters), and
#'   optionally 'expr' (a function to generate data).
#' @param alpha_level Numeric. The significance level for hypothesis tests. Default
#'   is 0.05.
#' @param verbose Logical. If TRUE, print progress messages. Default is FALSE.
#'
#' @return A data frame of class 'replext_gls' containing simulation results, including:
#'   \item{term}{Character. The name of the model term.}
#'   \item{tppp}{Integer. The number of timepoints per phase.}
#'   \item{rho}{Numeric. The autocorrelation parameter.}
#'   \item{success_rate}{Numeric. The proportion of successful model fits.}
#'   \item{mean_estimates}{Numeric. The mean of the estimated coefficients.}
#'   \item{mean_bias}{Numeric. The mean bias of the estimated coefficients.}
#'   \item{se_estimates}{Numeric. The standard error of the estimated coefficients.}
#'   \item{rejection_rates}{Numeric. The proportion of significant hypothesis tests.}
#'   \item{se_rejection_rates}{Numeric. The standard error of the rejection rates.}
#'   \item{mean_rmse}{Numeric. The mean root mean square error of the estimates.}
#'   \item{alpha_level}{Numeric. The significance level used for hypothesis tests.}
#'   \item{iterations}{Integer. The number of iterations performed.}
#'   \item{n_phases}{Integer. The number of phases in the design.}
#'   \item{n_IDs}{Integer. The number of subjects simulated.}
#'   \item{formula}{Character. The model formula used.}
#'   \item{covariate_specs}{Character. The covariate specifications used.}
#'
#' @references
#' Maric, M., de Haan, E., Hogendoorn, S.M., Wolters, L.H. & Huizenga, H.M. (2014).
#' Evaluating statistical and clinical significance of intervention effects in
#' single-case experimental designs: An SPSS method to analyze univariate data.
#' Behavior Therapy. doi: 10.1016/j.beth.2014.09.009
#'
#' @examples
#' results <- replext_gls(
#'   n_timepoints_list = c(10),
#'   rho_list = c(0.2),
#'   iterations = 10,
#'   betas = c("(Intercept)" = 0, "phase1" = 1),
#'   formula = y ~ phase
#' )
#'
#' @export
replext_gls <- function(n_timepoints_list, rho_list, iterations, n_phases = 2, n_IDs = 1,
                        betas, formula, covariate_specs = NULL, alpha_level = 0.05,
                        verbose = FALSE) {

  # generate a run_code
  run_code <- mmints::generateRunCode()

  results_df <- data.frame()
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
        res <- simulate_gls_once(n_timepoints_per_phase = n_tp,
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
      if (sum(success_list) > 0) {
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
        rejection_rates <- colMeans(p_values_matrix < alpha_level, na.rm = TRUE)
        # Compute standard error of rejection rates
        se_rejection_rates <- sqrt(rejection_rates * (1 - rejection_rates) / sum(success_list))

        # Get the term names
        terms <- names(mean_estimates)

        # Create a data frame for the current parameters
        df_current <- data.frame(
          term = terms,
          tppp = n_tp,
          rho = rho,
          success_rate = success_rate,
          mean_estimates = mean_estimates,
          mean_bias = mean_bias,
          se_estimates = se_estimates,
          rejection_rates = rejection_rates,
          se_rejection_rates = se_rejection_rates,
          mean_rmse = mean_rmse,
          alpha_level = alpha_level,
          iterations = iterations,
          n_phases = n_phases,
          n_IDs = n_IDs,
          formula = deparse1(formula, collapse = ""),
          betas = deparse1(betas, collapse = ""),
          covariate_specs = deparse1(covariate_specs, collapse = ""),
          run_code = run_code,
          stringsAsFactors = FALSE
        )
        # Append to results_df
        results_df <- rbind(results_df, df_current)
      } else {
        # If no successful iterations, create a data frame with NA values
        # We need to know the terms to create the data frame
        # Since no successful iterations, we can get the terms from the betas
        terms <- names(betas)

        df_current <- data.frame(
          term = terms,
          tppp = n_tp,
          rho = rho,
          success_rate = success_rate,
          mean_estimates = NA,
          mean_bias = NA,
          se_estimates = NA,
          rejection_rates = NA,
          se_rejection_rates = NA,
          mean_rmse = NA,
          alpha_level = alpha_level,
          iterations = iterations,
          n_phases = n_phases,
          n_IDs = n_IDs,
          formula = deparse1(formula, collapse = ""),
          betas = deparse1(betas, collapse = ""),
          covariate_specs = deparse1(covariate_specs, collapse = ""),
          run_code = run_code,
          stringsAsFactors = FALSE
        )
        # Append to results_df
        results_df <- rbind(results_df, df_current)
      }
    }
  }

  class(results_df) <- c("replext_gls", class(results_df))

  return(results_df)
}

#' Plot Results from Replications and Extension of GLS Simulation
#'
#' This method creates a ggplot2 visualization of results from a replext_gls object.
#' It allows for flexible plotting of different metrics across various conditions.
#'
#' @param x An object of class "replext_gls", typically the output from replext_gls().
#' @param term Character. The name of the model term to plot.
#' @param metric Character. The metric to plot on the y-axis. Default is "rejection_rates".
#'   Other options include "mean_estimates", "mean_bias", etc.
#' @param x_axis Character. The variable to plot on the x-axis. Default is "tppp"
#'   (time points per phase).
#' @param color Character. The variable to use for color grouping. Default is "rho"
#'   (autocorrelation parameter).
#' @param title Character. The plot title. If NULL, a default title is generated.
#' @param x_label Character. The x-axis label. If NULL, a default label is generated.
#' @param y_label Character. The y-axis label. If NULL, a default label is generated.
#' @param ... Pass extra arguments to ggplot.
#'
#' @return A ggplot2 object representing the plot.
#'
#' @details
#' This method creates a line plot with points, where each line represents a different
#' level of the color variable (default is rho). If the metric is "rejection_rates",
#' error bars are added to represent the standard error of the rejection rates.
#'
#' @examples
#' results <- replext_gls(
#'   n_timepoints_list = c(10),
#'   rho_list = c(0.2),
#'   iterations = 10,
#'   betas = c("(Intercept)" = 0, "phase1" = 1),
#'   formula = y ~ phase
#' )
#' plot(results, term = "phase1", metric = "rejection_rates")
#'
#' @method plot replext_gls
#' @export
plot.replext_gls <- function(x, term, metric = "rejection_rates",
                             x_axis = "tppp", color = "rho",
                             title = NULL, x_label = NULL, y_label = NULL, ...) {

  # Check if the term exists in the data
  if (!term %in% unique(x$term)) {
    stop("The specified term does not exist in the data.")
  }

  # Check if the metric exists in the data
  if (!metric %in% names(x)) {
    stop("The specified metric does not exist in the data.")
  }

  # Filter data for the specified term
  data <- x[x$term == term, ]

  # Convert color to a factor
  if (!is.factor(data[[color]])){
    data[[color]] <- as.factor(data[[color]])
  }

  # Set up labels
  if (is.null(title)) {
    title <- paste("Plot of", metric, "for term:", term)
  }
  if (is.null(x_label)) {
    x_label <- ifelse(x_axis == "tppp", "Number of observations", x_axis)
  }
  if (is.null(y_label)) {
    y_label <- gsub("_", " ", metric)
    y_label <- paste0(toupper(substr(y_label, 1, 1)), substr(y_label, 2, nchar(y_label)))
  }

  # Create the plot
  p <- ggplot2::ggplot(data,
                       ggplot2::aes_string(x = x_axis, y = metric,
                                           color = color, group = color),
                       ...) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::labs(title = title, x = x_label, y = y_label) +
    ggplot2::theme_minimal()

  return(p)
}
