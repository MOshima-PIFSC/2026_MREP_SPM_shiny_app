# Surplus Production Stock Assessment Model
# Fits Schaefer model to catch and effort data from workshop

#' Fit Schaefer Surplus Production Model
#'
#' Estimates stock parameters (r, K, q) from catch and effort time series
#' using non-linear optimization
#'
#' @param data Data frame with columns: Year, Count, Effort (and optionally CPUE)
#' @param initial_params Named vector of starting values for optimization
#' @param method Optimization method (default: "L-BFGS-B")
#' @param use_cpue If TRUE, fit to CPUE data; if FALSE, fit to catch (default: TRUE)
#'
#' @return List containing model fit results, parameters, and reference points
fit_schaefer_model <- function(data,
                               initial_params = NULL,
                               method = "L-BFGS-B",
                               use_cpue = TRUE) {
  
  # Check data quality first
  data_check <- check_data_quality(data)
  if (data_check$has_issues) {
    stop("Data quality issues detected. Please fix before fitting.")
  }
  
  # Extract data vectors
  years <- data$Date
  catch <- data$Count
  effort <- data$Effort
  n_years <- length(years)
  
  # Calculate CPUE if not provided
  if (!"CPUE" %in% names(data)) {
    data$CPUE <- catch / effort
  }
  cpue_obs <- data$CPUE
  
  # Set initial parameter values if not provided
  if (is.null(initial_params)) {
    # Intelligent starting values based on data
    max_catch <- max(catch, na.rm = TRUE)
    mean_catch <- mean(catch, na.rm = TRUE)
    mean_cpue <- mean(cpue_obs, na.rm = TRUE)
    
    # Safely estimate CPUE trend
    cpue_trend <- tryCatch({
      lm(cpue_obs ~ years)$coefficients[2]
    }, error = function(e) {
      0  # Default to no trend if calculation fails
    })
    
    # Handle NA or invalid trend
    if (is.na(cpue_trend)) cpue_trend <- 0
    
    # Estimate K from catch data
    # Rough rule: MSY ≈ 0.4 * mean catch for developing fishery
    # MSY = rK/4, so K = 4*MSY/r
    # Initial guess: r ≈ 0.2-0.3 for most stocks
    K_guess <- (mean_catch * 2) / 0.15  # Conservative estimate
    
    # Estimate q from CPUE
    # CPUE = q*B, and if stock near K/2, then q ≈ CPUE/(K/2)
    q_guess <- mean_cpue / (K_guess * 0.5)
    q_guess <- max(q_guess, 0.00001)  # Ensure positive
    
    # Estimate r from CPUE trend
    # Declining CPUE suggests higher r needed to compensate
    r_guess <- if (cpue_trend < 0) 0.3 else 0.2
    
    initial_params <- c(
      r = r_guess,
      K = K_guess,
      q = q_guess
    )
    
    cat("Initial parameter guesses:\n")
    cat(sprintf("  r = %.4f\n", r_guess))
    cat(sprintf("  K = %.0f\n", K_guess))
    cat(sprintf("  q = %.6f\n\n", q_guess))
  }
  
  # Set parameter bounds
  lower_bounds <- c(
    r = 0.01,
    K = max(catch) * 2,
    q = 0.000001
  )
  
  upper_bounds <- c(
    r = 2.0,
    K = max(catch) * 50,
    q = 0.1
  )
  
  # Define objective function (negative log-likelihood)
  objective_function <- function(params) {
    
    r <- params[1]
    K <- params[2]
    q <- params[3]
    
    # Initialize biomass vector
    B <- numeric(n_years)
    B[1] <- K * 0.8  # Start at 80% of K
    
    # Predicted values
    cpue_pred <- numeric(n_years)
    catch_pred <- numeric(n_years)
    
    # Forward simulation
    for (t in 1:n_years) {
      
      # Predicted CPUE
      cpue_pred[t] <- q * B[t]
      
      # Predicted catch
      F_rate <- q * effort[t]
      if (F_rate > 0 && B[t] > 0) {
        catch_pred[t] <- (F_rate / (F_rate + 0.01)) * B[t] * (1 - exp(-(F_rate + 0.01)))
      } else {
        catch_pred[t] <- 0
      }
      
      # Update biomass for next year
      if (t < n_years) {
        surplus <- r * B[t] * (1 - B[t] / K)
        B[t + 1] <- B[t] + surplus - catch[t]
        
        # Prevent negative or extreme biomass
        B[t + 1] <- max(B[t + 1], K * 0.01)
        B[t + 1] <- min(B[t + 1], K * 1.5)
      }
    }
    
    # Calculate negative log-likelihood
    if (use_cpue) {
      # Fit to CPUE (more stable for workshop data)
      residuals <- log(cpue_obs + 0.0001) - log(cpue_pred + 0.0001)
      nll <- sum(residuals^2, na.rm = TRUE)
    } else {
      # Fit to catch
      residuals <- log(catch + 1) - log(catch_pred + 1)
      nll <- sum(residuals^2, na.rm = TRUE)
    }
    
    # Add penalty for unrealistic parameters
    if (r < 0.01 || r > 2 || K < max(catch) || q < 0 || q > 1) {
      nll <- nll + 1000
    }
    
    return(nll)
  }
  
  # Try multiple optimization strategies for robustness
  cat("Attempting model fit...\n")
  
  best_fit <- NULL
  best_value <- Inf
  
  # Strategy 1: L-BFGS-B with tight tolerance
  cat("  Method 1: L-BFGS-B...\n")
  fit1 <- try(optim(
    par = initial_params,
    fn = objective_function,
    method = "L-BFGS-B",
    lower = lower_bounds,
    upper = upper_bounds,
    control = list(maxit = 3000, factr = 1e10, pgtol = 1e-8)
  ), silent = TRUE)
  
  if (!inherits(fit1, "try-error") && fit1$value < best_value) {
    best_fit <- fit1
    best_value <- fit1$value
    cat("    Objective value:", fit1$value, "Convergence:", fit1$convergence, "\n")
  }
  
  # Strategy 2: Try with different starting values
  if (is.null(best_fit) || best_fit$convergence != 0) {
    cat("  Method 2: L-BFGS-B with alternative starting values...\n")
    alt_params <- initial_params * c(1.5, 0.8, 1.2)  # Perturb starting values
    fit2 <- try(optim(
      par = alt_params,
      fn = objective_function,
      method = "L-BFGS-B",
      lower = lower_bounds,
      upper = upper_bounds,
      control = list(maxit = 3000, factr = 1e10)
    ), silent = TRUE)
    
    if (!inherits(fit2, "try-error") && fit2$value < best_value) {
      best_fit <- fit2
      best_value <- fit2$value
      cat("    Objective value:", fit2$value, "Convergence:", fit2$convergence, "\n")
    }
  }
  
  # Strategy 3: Nelder-Mead (no gradients, more robust)
  if (is.null(best_fit) || best_fit$convergence != 0) {
    cat("  Method 3: Nelder-Mead...\n")
    start_params <- if (!is.null(best_fit)) best_fit$par else initial_params
    fit3 <- try(optim(
      par = start_params,
      fn = objective_function,
      method = "Nelder-Mead",
      control = list(maxit = 5000, reltol = 1e-10)
    ), silent = TRUE)
    
    if (!inherits(fit3, "try-error") && fit3$value < best_value) {
      best_fit <- fit3
      best_value <- fit3$value
      cat("    Objective value:", fit3$value, "Convergence:", fit3$convergence, "\n")
    }
  }
  
  # Strategy 4: BFGS (unconstrained, then check bounds)
  if (is.null(best_fit) || best_fit$convergence != 0) {
    cat("  Method 4: BFGS...\n")
    start_params <- if (!is.null(best_fit)) best_fit$par else initial_params
    fit4 <- try(optim(
      par = start_params,
      fn = objective_function,
      method = "BFGS",
      control = list(maxit = 2000)
    ), silent = TRUE)
    
    if (!inherits(fit4, "try-error") && fit4$value < best_value) {
      # Check if parameters are within bounds
      in_bounds <- all(fit4$par >= lower_bounds & fit4$par <= upper_bounds)
      if (in_bounds) {
        best_fit <- fit4
        best_value <- fit4$value
        cat("    Objective value:", fit4$value, "Convergence:", fit4$convergence, "\n")
      }
    }
  }
  
  # Use best fit found
  if (is.null(best_fit)) {
    stop("All optimization methods failed")
  }
  
  fit <- best_fit
  
  # Report convergence status
  if (fit$convergence == 0) {
    cat("\n", clisymbols::symbol$tick, " Model converged successfully!\n")
    cat("  Final objective value:", fit$value, "\n\n")
  } else {
    cat("\n", clisymbols::symbol$warning, " Model convergence uncertain (code:", fit$convergence, ")\n")
    cat("  Final objective value:", fit$value, "\n")
    cat("  Parameter estimates may be approximate\n\n")
  }
  
  tryCatch({
    
    # Extract estimated parameters
    r_est <- fit$par[1]
    K_est <- fit$par[2]
    q_est <- fit$par[3]
    
    # Calculate reference points
    Bmsy <- unname(K_est / 2)
    MSY <- unname(r_est * K_est / 4)
    Fmsy <- unname(r_est / 2)
    
    # Run forward simulation with estimated parameters to get biomass trajectory
    B_est <- numeric(n_years)
    B_est[1] <- K_est * 0.8
    cpue_pred <- numeric(n_years)
    catch_pred <- numeric(n_years)
    F_est <- numeric(n_years)
    
    for (t in 1:n_years) {
      cpue_pred[t] <- q_est * B_est[t]
      F_est[t] <- q_est * effort[t]
      
      if (F_est[t] > 0 && B_est[t] > 0) {
        catch_pred[t] <- (F_est[t] / (F_est[t] + 0.01)) * B_est[t] * (1 - exp(-(F_est[t] + 0.01)))
      } else {
        catch_pred[t] <- 0
      }
      
      if (t < n_years) {
        surplus <- r_est * B_est[t] * (1 - B_est[t] / K_est)
        B_est[t + 1] <- B_est[t] + surplus - catch[t]
        B_est[t + 1] <- max(B_est[t + 1], K_est * 0.01)
        B_est[t + 1] <- min(B_est[t + 1], K_est * 1.5)
      }
    }
    
    # Calculate current status (terminal year)
    B_current <- B_est[n_years]
    F_current <- F_est[n_years]
    
    B_Bmsy <- B_current / Bmsy
    F_Fmsy <- F_current / Fmsy
    
    # Determine stock status
    stock_status <- get_stock_status(B_Bmsy, F_Fmsy)
    
    # Calculate residuals and fit statistics
    residuals <- cpue_obs - cpue_pred
    rmse <- sqrt(mean(residuals^2, na.rm = TRUE))
    r_squared <- 1 - (sum(residuals^2, na.rm = TRUE) / 
                      sum((cpue_obs - mean(cpue_obs, na.rm = TRUE))^2, na.rm = TRUE))
    
    # Create results object
    results <- list(
      # Model fit
      convergence = fit$convergence,
      iterations = fit$counts,
      objective_value = fit$value,
      
      # Estimated parameters
      parameters = list(
        r = r_est,
        K = K_est,
        q = q_est
      ),
      
      # Reference points
      reference_points = list(
        Bmsy = Bmsy,
        MSY = MSY,
        Fmsy = Fmsy
      ),
      
      # Current status
      current_status = list(
        B_current = B_current,
        F_current = F_current,
        B_Bmsy = B_Bmsy,
        F_Fmsy = F_Fmsy,
        status = stock_status
      ),
      
      # Time series
      time_series = data.frame(
        Year = as.numeric(years),
        Catch_obs = catch,
        Catch_pred = catch_pred,
        Effort = effort,
        CPUE_obs = cpue_obs,
        CPUE_pred = cpue_pred,
        Biomass = B_est,
        F_rate = F_est
      ),
      
      # Model diagnostics
      diagnostics = list(
        rmse = rmse,
        r_squared = r_squared,
        residuals = residuals
      )
    )
    
    class(results) <- c("schaefer_fit", "list")
    
    return(results)
    
  }, error = function(e) {
    warning("Model fitting failed: ", e$message)
    cat("\nTrying simplified fitting approach...\n")
    
    # Simplified approach: Fix q and estimate r and K only
    tryCatch({
      simple_fit <- fit_simple_schaefer(data)
      return(simple_fit)
    }, error = function(e2) {
      warning("Simplified fitting also failed: ", e2$message)
      return(NULL)
    })
  })
}

#' Simplified Schaefer Model (2-parameter)
#'
#' Fixes catchability (q) and estimates only r and K
#' More stable for short or noisy time series
#'
#' @param data Data frame with Year, Count, Effort
#'
#' @return Schaefer fit object
fit_simple_schaefer <- function(data) {
  
  years <- data$Date
  catch <- data$Count
  effort <- data$Effort
  n_years <- length(years)
  
  cpue_obs <- catch / effort
  mean_cpue <- mean(cpue_obs)
  
  # Fix q at a reasonable value
  q_fixed <- mean_cpue / (mean(catch) * 2)
  
  cat(sprintf("Using simplified 2-parameter model with q fixed at %.6f\n", q_fixed))
  
  # Objective function with fixed q
  obj_simple <- function(params) {
    r <- params[1]
    K <- params[2]
    
    B <- numeric(n_years)
    B[1] <- K * 0.8
    cpue_pred <- numeric(n_years)
    
    for (t in 1:n_years) {
      cpue_pred[t] <- q_fixed * B[t]
      
      if (t < n_years) {
        surplus <- r * B[t] * (1 - B[t] / K)
        B[t + 1] <- B[t] + surplus - catch[t]
        B[t + 1] <- max(B[t + 1], K * 0.01)
        B[t + 1] <- min(B[t + 1], K * 1.5)
      }
    }
    
    residuals <- log(cpue_obs + 0.0001) - log(cpue_pred + 0.0001)
    return(sum(residuals^2, na.rm = TRUE))
  }
  
  # Optimize
  fit <- optim(
    par = c(r = 0.3, K = mean(catch) * 10),
    fn = obj_simple,
    method = "L-BFGS-B",
    lower = c(0.01, max(catch) * 2),
    upper = c(2.0, max(catch) * 50),
    control = list(maxit = 2000)
  )
  
  # Build results object similar to full model
  r_est <- fit$par[1]
  K_est <- fit$par[2]
  q_est <- q_fixed
  
  # Continue with same result structure as full model...
  Bmsy <- K_est / 2
  MSY <- r_est * K_est / 4
  Fmsy <- r_est / 2
  
  # Forward simulation for results
  B_est <- numeric(n_years)
  B_est[1] <- K_est * 0.8
  cpue_pred <- numeric(n_years)
  F_est <- numeric(n_years)
  catch_pred <- numeric(n_years)
  
  for (t in 1:n_years) {
    cpue_pred[t] <- q_est * B_est[t]
    F_est[t] <- q_est * effort[t]
    
    if (F_est[t] > 0 && B_est[t] > 0) {
      catch_pred[t] <- (F_est[t] / (F_est[t] + 0.01)) * B_est[t] * (1 - exp(-(F_est[t] + 0.01)))
    } else {
      catch_pred[t] <- 0
    }
    
    if (t < n_years) {
      surplus <- r_est * B_est[t] * (1 - B_est[t] / K_est)
      B_est[t + 1] <- B_est[t] + surplus - catch[t]
      B_est[t + 1] <- max(B_est[t + 1], K_est * 0.01)
      B_est[t + 1] <- min(B_est[t + 1], K_est * 1.5)
    }
  }
  
  B_current <- B_est[n_years]
  F_current <- F_est[n_years]
  B_Bmsy <- B_current / Bmsy
  F_Fmsy <- F_current / Fmsy
  stock_status <- get_stock_status(B_Bmsy, F_Fmsy)
  
  residuals <- cpue_obs - cpue_pred
  rmse <- sqrt(mean(residuals^2, na.rm = TRUE))
  r_squared <- 1 - (sum(residuals^2, na.rm = TRUE) / 
                    sum((cpue_obs - mean(cpue_obs, na.rm = TRUE))^2, na.rm = TRUE))
  
  results <- list(
    convergence = fit$convergence,
    iterations = fit$counts,
    objective_value = fit$value,
    model_type = "simplified (2-parameter, q fixed)",
    
    parameters = list(
      r = r_est,
      K = K_est,
      q = q_est
    ),
    
    reference_points = list(
      Bmsy = Bmsy,
      MSY = MSY,
      Fmsy = Fmsy
    ),
    
    current_status = list(
      B_current = B_current,
      F_current = F_current,
      B_Bmsy = B_Bmsy,
      F_Fmsy = F_Fmsy,
      status = stock_status
    ),
    
    time_series = data.frame(
      Year = years,
      Catch_obs = catch,
      Catch_pred = catch_pred,
      Effort = effort,
      CPUE_obs = cpue_obs,
      CPUE_pred = cpue_pred,
      Biomass = B_est,
      F_rate = F_est
    ),
    
    diagnostics = list(
      rmse = rmse,
      r_squared = r_squared,
      residuals = residuals
    )
  )
  
  class(results) <- c("schaefer_fit", "list")
  return(results)
}

#' Determine Stock Status
#'
#' Classifies stock status based on B/Bmsy and F/Fmsy ratios
#'
#' @param B_Bmsy Ratio of current biomass to Bmsy
#' @param F_Fmsy Ratio of current fishing mortality to Fmsy
#'
#' @return Character string describing stock status
get_stock_status <- function(B_Bmsy, F_Fmsy) {
  
  if (B_Bmsy > 1 && F_Fmsy < 1) {
    status <- "Healthy: Not overfished, not experiencing overfishing"
  } else if (B_Bmsy > 1 && F_Fmsy > 1) {
    status <- "Overfishing occurring: Stock healthy but fishing pressure too high"
  } else if (B_Bmsy < 1 && F_Fmsy < 1) {
    status <- "Overfished: Stock depleted but fishing pressure reduced"
  } else {
    status <- "Overfished and overfishing: Stock depleted and fishing pressure too high"
  }
  
  return(status)
}

#' Print Schaefer Model Results
#'
#' @param fit Schaefer model fit object
print.schaefer_fit <- function(fit) {
  
  cat("=== SCHAEFER SURPLUS PRODUCTION MODEL RESULTS ===\n\n")
  
  cat("MODEL FIT:\n")
  cat(sprintf("  Convergence: %s\n", ifelse(fit$convergence == 0, "SUCCESS", "FAILED")))
  cat(sprintf("  R-squared: %.3f\n", fit$diagnostics$r_squared))
  cat(sprintf("  RMSE: %.4f\n\n", fit$diagnostics$rmse))
  
  cat("ESTIMATED PARAMETERS:\n")
  cat(sprintf("  r (growth rate): %.4f\n", fit$parameters$r))
  cat(sprintf("  K (carrying capacity): %.0f\n", fit$parameters$K))
  cat(sprintf("  q (catchability): %.6f\n\n", fit$parameters$q))
  
  cat("REFERENCE POINTS:\n")
  cat(sprintf("  MSY (Maximum Sustainable Yield): %.0f\n", fit$reference_points$MSY))
  cat(sprintf("  Bmsy (Biomass at MSY): %.0f\n", fit$reference_points$Bmsy))
  cat(sprintf("  Fmsy (Fishing mortality at MSY): %.4f\n\n", fit$reference_points$Fmsy))
  
  cat("CURRENT STOCK STATUS:\n")
  cat(sprintf("  Current Biomass: %.0f\n", fit$current_status$B_current))
  cat(sprintf("  Current F: %.4f\n", fit$current_status$F_current))
  cat(sprintf("  B/Bmsy: %.2f %s\n", fit$current_status$B_Bmsy,
              ifelse(fit$current_status$B_Bmsy > 1, clisymbols::symbol$tick, clisymbols::symbol$cross)))
  cat(sprintf("  F/Fmsy: %.2f %s\n", fit$current_status$F_Fmsy,
              ifelse(fit$current_status$F_Fmsy < 1, clisymbols::symbol$tick, clisymbols::symbol$cross)))
  cat(sprintf("  Status: %s\n\n", fit$current_status$status))
  
  cat("MANAGEMENT ADVICE:\n")
  if (fit$current_status$B_Bmsy < 0.5) {
    cat("  ", clisymbols::symbol$warning, "CRITICAL: Stock severely depleted. Immediate reduction in fishing needed.\n")
    cat(sprintf("  Recommended catch: %.0f (50%% of recent average)\n", 
                mean(tail(fit$time_series$Catch_obs, 3)) * 0.5))
  } else if (fit$current_status$B_Bmsy < 1.0) {
    cat("  ", clisymbols::symbol$warning, "WARNING: Stock below target level. Reduce fishing pressure.\n")
    cat(sprintf("  Recommended catch: %.0f (70%% of MSY)\n", 
                fit$reference_points$MSY * 0.7))
  } else if (fit$current_status$F_Fmsy > 1.2) {
    cat("  ", clisymbols::symbol$warning, "CAUTION: Fishing pressure too high. Reduce effort.\n")
    cat(sprintf("  Recommended catch: %.0f (MSY)\n", fit$reference_points$MSY))
  } else {
    cat("  ", clisymbols::symbol$tick, "Stock in good condition. Maintain current fishing levels.\n")
    cat(sprintf("  Recommended catch: %.0f (MSY)\n", fit$reference_points$MSY))
  }
  
  cat("\n")
}

#' Plot Model Fit to Data
#'
#' @param fit Schaefer model fit object
#'
#' @return ggplot object
plot_model_fit <- function(fit) {
  
  ts_data <- fit$time_series
  
  # Plot observed vs predicted CPUE
  p <- ggplot(ts_data, aes(x = Year)) +
    geom_point(aes(y = CPUE_obs, color = "Observed"), size = 3) +
    geom_line(aes(y = CPUE_pred, color = "Model"), linewidth = 1.2) +
    scale_color_manual(values = c("Observed" = "#E63946", "Model" = "#457B9D")) +
    labs(
      title = "Model Fit: Observed vs Predicted CPUE",
      subtitle = sprintf("R² = %.3f", fit$diagnostics$r_squared),
      y = "CPUE (Catch per Unit Effort)",
      color = ""
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold")
    )
  
  return(p)
}

#' Plot Estimated Biomass Trajectory
#'
#' @param fit Schaefer model fit object
#'
#' @return ggplot object
plot_biomass_trajectory <- function(fit) {
  
  ts_data <- fit$time_series
  Bmsy <- fit$reference_points$Bmsy
  K <- fit$parameters$K
  B_Bmsy <- fit$current_status$B_Bmsy
  
  p <- ggplot(ts_data, aes(x = Year, y = Biomass)) +
    geom_hline(yintercept = Bmsy, linetype = "solid", color = "darkgreen", linewidth = 1) +
    #geom_hline(yintercept = 0.5, linetype = "dashed", color = "orange", linewidth = 0.8) +
    geom_ribbon(aes(ymin = 0, ymax = Bmsy), fill = "orange", alpha = 0.2) +
    geom_ribbon(aes(ymin = Bmsy, ymax = Inf), fill = "green", alpha = 0.2) +
    #geom_ribbon(aes(ymin = 1.0, ymax = Biomass/Bmsy), fill = "green", alpha = 0.2) +
    geom_line(linewidth = 1.5, color = "#1d3557") +
    geom_point(size = 3, color = "#1d3557") +
    geom_point(data = tail(ts_data, 1), aes(y = Biomass), 
               size = 5, color = "red", shape = 18) +
    annotate("text", x = min(ts_data$Year), y = Bmsy, label = "Bmsy (Target)", 
             vjust = -0.5, hjust = 0, color = "darkgreen", fontface = "bold") +
    # annotate("text", x = min(ts_data$Year), y = 1, label = "Overfished", 
    #          vjust = 1.5, hjust = 0, color = "darkred", fontface = "bold") +
    labs(
      title = "Estimated Stock Biomass Over Time",
      subtitle = sprintf("Current Status: B/Bmsy = %.2f", B_Bmsy),
      y = "Biomass",
      caption = "Green zone = Healthy | Yellow = Overfished"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"))
  
  return(p)
}

#' Create Kobe Plot
#'
#' @param fit Schaefer model fit object
#'
#' @return ggplot object
plot_kobe <- function(fit) {
  
  B_Bmsy <- fit$current_status$B_Bmsy
  F_Fmsy <- fit$current_status$F_Fmsy
  
  # Create trajectory if time series available
  ts_data <- fit$time_series
  ts_data$B_Bmsy <- ts_data$Biomass / fit$reference_points$Bmsy
  ts_data$F_Fmsy <- ts_data$F_rate / fit$reference_points$Fmsy
  
  p <- ggplot() +
    # Quadrant shading
    annotate("rect", xmin = 0, xmax = 1, ymin = 1, ymax = Inf, 
             fill = "red", alpha = 0.3) +
    annotate("rect", xmin = 1, xmax = Inf, ymin = 1, ymax = Inf, 
             fill = "yellow", alpha = 0.3) +
    annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1, 
             fill = "orange", alpha = 0.3) +
    annotate("rect", xmin = 1, xmax = Inf, ymin = 0, ymax = 1, 
             fill = "green", alpha = 0.3) +
    
    # Reference lines
    geom_hline(yintercept = 1, linetype = "dashed", linewidth = 1) +
    geom_vline(xintercept = 1, linetype = "dashed", linewidth = 1) +
    
    # Trajectory
    geom_path(data = ts_data, aes(x = B_Bmsy, y = F_Fmsy), 
              linewidth = 1, color = "blue", alpha = 0.6) +
    geom_point(data = ts_data, aes(x = B_Bmsy, y = F_Fmsy), 
               size = 2, color = "blue", alpha = 0.6) +
    
    # Current position
    geom_point(aes(x = B_Bmsy, y = F_Fmsy), 
               size = 6, color = "darkred", shape = 18) +
    annotate("text", x = B_Bmsy, y = F_Fmsy, 
             label = "Current", vjust = -1.5, fontface = "bold") +
    
    # Labels
    annotate("text", x = 0.3, y = 1.7, label = "Overfished\n& Overfishing", 
             fontface = "bold", size = 5, color = "darkred") +
    annotate("text", x = 1.5, y = 1.7, label = "Overfishing\n(stock still healthy)", 
             fontface = "bold", size = 4) +
    annotate("text", x = 0.4, y = 0.3, label = "Overfished \n(but sustainable fishing)", 
             fontface = "bold", size = 4) +
    annotate("text", x = 1.5, y = 0.3, label = "Healthy", 
             fontface = "bold", size = 5, color = "darkgreen") +
    
    coord_cartesian(xlim = c(0, max(2, B_Bmsy * 1.2)), 
                    ylim = c(0, max(2, F_Fmsy * 1.2))) +
    labs(
      title = "Kobe Plot: Stock Status",
      subtitle = sprintf("B/Bmsy = %.2f | F/Fmsy = %.2f", B_Bmsy, F_Fmsy),
      x = "B / Bmsy (Stock Size)",
      y = "F / Fmsy (Fishing Pressure)"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"))
  
  return(p)
}


# =============================================================================
# EXAMPLE USAGE
# =============================================================================

# cat("=== STOCK ASSESSMENT MODEL EXAMPLE ===\n\n")

# # First generate some data (or load from previous example)
# source("fish_data_gen.R")  # Assumes previous code is saved

# # Generate historical + workshop data
# cat("Generating historical data...\n")
# historical <- generate_historical_data(
#   n_years = 12,
#   scenario = "overfished",  # Try an overfished stock
#   K = 300000,
#   r = 0.18,
#   start_year = 2012,
#   seed = 42
# )

# # Simulate 3 additional workshop years
# cat("Simulating workshop catch data...\n")
# workshop_data <- data.frame(
#   Year = 2024:2026,
#   Catch = c(32, 27, 15)
# )

# workshop_scaled <- process_workshop_simple(workshop_data, historical)

# combined_data_correct <- rbind(
#   historical %>% select(Year, Catch, Effort),
#   workshop_scaled %>% select(Year, Catch, Effort)
# )


# cat(sprintf("Total years of data: %d\n\n", nrow(combined_data)))

# # Fit the model
# cat("Fitting Schaefer surplus production model...\n\n")

# model_fit <- fit_schaefer_model(
#   data = historical,
#   use_cpue = TRUE
# )

# # Display results
# print(model_fit)

# # Create plots
# cat("Generating diagnostic plots...\n\n")

# p1 <- plot_model_fit(model_fit)
# print(p1)

# p2 <- plot_biomass_trajectory(model_fit)
# print(p2)

# p3 <- plot_kobe(model_fit)
# print(p3)

# cat("\nStock assessment complete!\n")