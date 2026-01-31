# Historical Fisheries Data Generator for Workshop
# Generates realistic catch and effort data based on surplus production dynamics

library(dplyr)
library(ggplot2)

#' Generate Historical Fisheries Data
#' 
#' Creates synthetic catch and effort data following Schaefer surplus production model
#' with realistic variability and different stock status scenarios. This version ensures
#' the FINAL biomass matches the scenario (e.g., healthy stocks end healthy).
#'
#' @param n_years Number of historical years to generate (default: 12)
#' @param scenario Stock status scenario: "healthy", "overfished", "recovering", or "declining"
#' @param K Carrying capacity (unfished biomass)
#' @param r Intrinsic growth rate
#' @param q Catchability coefficient
#' @param start_year Starting year for the time series
#' @param catch_cv Coefficient of variation for catch observation error
#' @param effort_cv Coefficient of variation for effort variability
#' @param seed Random seed for reproducibility (optional)
#'
#' @return Data frame with columns: Year, Biomass, Catch, Effort, CPUE
generate_historical_data <- function(n_years = 12,
                                     scenario = "healthy",
                                     K = 1000,
                                     r = 0.3,
                                     q = 0.0001,
                                     start_year = 2012,
                                     catch_cv = 0.15,
                                     effort_cv = 0.20,
                                     seed = NULL) {
  
  if (!is.null(seed)) set.seed(seed)
  
  # Calculate reference points
  Bmsy <- K / 2
  MSY <- r * K / 4
  Fmsy <- r / 2
  
  # Get scenario-specific targets
  scenario_params <- get_scenario_params(scenario, K, Bmsy)
  
  # Initialize vectors
  years <- start_year:(start_year + n_years - 1)
  B <- numeric(n_years)
  C <- numeric(n_years)
  E <- numeric(n_years)
  F_rate <- numeric(n_years)
  
  # Set initial and final biomass targets
  B_initial <- scenario_params$B_initial
  B_final <- scenario_params$B_final
  
  # Create biomass trajectory that goes from B_initial to B_final
  B <- generate_biomass_trajectory(n_years, B_initial, B_final, K, r, 
                                   scenario_params$trajectory_type)
  
  # Calculate effort needed to produce this biomass trajectory
  # Working backwards from desired biomass and surplus production
  for (t in 1:(n_years-1)) {
    
    # Surplus production at current biomass
    surplus <- r * B[t] * (1 - B[t] / K)
    
    # Catch needed to get from B[t] to B[t+1]
    # B[t+1] = B[t] + surplus - C[t]
    # So: C[t] = B[t] + surplus - B[t+1]
    C[t] <- B[t] + surplus - B[t + 1]
    
    # Ensure catch is non-negative and reasonable
    C[t] <- max(C[t], 0)
    C[t] <- min(C[t], B[t] * 0.8)  # Don't remove more than 80% of biomass in one year
    
  }
  
  # Handle final year catch
  surplus_final <- r * B[n_years] * (1 - B[n_years] / K)
  C[n_years] <- min(surplus_final * 0.7, B[n_years] * 0.4)  # Conservative final catch
  C[n_years] <- max(C[n_years], 0)
  
  # Calculate effort from catch and biomass
  # Using: C = q * E * B (simplified catch equation)
  # So: E = C / (q * B)
  for (t in 1:n_years) {
    if (B[t] > 0 && C[t] > 0) {
      E[t] <- C[t] / (q * B[t])
    } else {
      E[t] <- scenario_params$base_effort
    }
  }
  
  # Add variability to effort to make it more realistic
  E <- E * rlnorm(n_years, meanlog = 0, sdlog = effort_cv)
  
  # Recalculate catch with variable effort (adds realism)
  for (t in 1:n_years) {
    F_rate[t] <- q * E[t]
    
    # Baranov catch equation
    if (F_rate[t] > 0) {
      C[t] <- (F_rate[t] / (F_rate[t] + 0.01)) * B[t] * (1 - exp(-(F_rate[t] + 0.01)))
    }
    
    # Add observation error to catch
    C[t] <- C[t] * rlnorm(1, meanlog = 0, sdlog = catch_cv)
    
    # Ensure catch doesn't exceed biomass
    C[t] <- min(C[t], B[t] * 0.9)
  }
  
  # Calculate CPUE
  CPUE <- C / E
  
  # Create output data frame
  data <- data.frame(
    Year = years,
    Biomass = round(B, 1),
    Catch = round(C, 1),
    Effort = round(E, 1),
    CPUE = round(CPUE, 4)
  )
  
  # Add reference points as attributes
  attr(data, "K") <- K
  attr(data, "r") <- r
  attr(data, "q") <- q
  attr(data, "MSY") <- MSY
  attr(data, "Bmsy") <- Bmsy
  attr(data, "Fmsy") <- Fmsy
  attr(data, "scenario") <- scenario
  attr(data, "final_B_Bmsy") <- B[n_years] / Bmsy
  
  return(data)
}

#' Generate Biomass Trajectory
#'
#' Creates a biomass time series from initial to final state
#' Trajectory shape depends on r (species with higher r change faster)
#'
#' @param n_years Number of years
#' @param B_initial Starting biomass
#' @param B_final Ending biomass
#' @param K Carrying capacity
#' @param r Growth rate (affects trajectory curvature)
#' @param trajectory_type Type of trajectory: "linear", "smooth", "fluctuating"
#'
#' @return Vector of biomass values
generate_biomass_trajectory <- function(n_years, B_initial, B_final, K, r,
                                       trajectory_type = "smooth") {
  
  t <- 1:n_years
  
  if (trajectory_type == "linear") {
    # Simple linear trajectory
    B <- seq(B_initial, B_final, length.out = n_years)
    
  } else if (trajectory_type == "smooth") {
    # Trajectory shape depends on r
    # High r species (like Mahi) change faster (steeper curve)
    # Low r species (like Opakapaka) change slower (gentler curve)
    
    # Scale curve steepness by r
    # r = 0.18 (Opaka): gentle curve (steepness ~ 2)
    # r = 0.35 (Tuna): moderate curve (steepness ~ 3)
    # r = 1.20 (Mahi): steep curve (steepness ~ 5)
    steepness <- 1 + (r / 0.3) * 2
    steepness <- max(1.5, min(steepness, 6))  # Bound between 1.5 and 6
    
    # Create sigmoid from 0 to 1
    x <- seq(-steepness, steepness, length.out = n_years)
    sigmoid <- 1 / (1 + exp(-x))
    
    # Scale sigmoid to go from B_initial to B_final
    # This works for both recovery (B_final > B_initial) and decline (B_final < B_initial)
    B <- B_initial + (B_final - B_initial) * sigmoid
    
  } else if (trajectory_type == "fluctuating") {
    # Add some realistic fluctuation around trend
    # Fluctuation magnitude also depends on r (higher r = more variable)
    base_trend <- seq(B_initial, B_final, length.out = n_years)
    
    # Fluctuation amplitude scales with r
    fluctuation_amplitude <- (B_initial - B_final) * (r / 0.5) * 0.15
    fluctuation_amplitude <- min(abs(fluctuation_amplitude), abs(B_initial - B_final) * 0.3)
    
    # Multiple frequency components for realistic pattern
    fluctuation <- fluctuation_amplitude * (
      0.6 * sin(2 * pi * t / 4) +           # 4-year cycle
      0.3 * sin(2 * pi * t / 2.5) +         # 2.5-year cycle
      0.1 * sin(2 * pi * t / 7)             # 7-year cycle
    )
    
    B <- base_trend + fluctuation
    
  } else {
    # Default to smooth
    x <- seq(-3, 3, length.out = n_years)
    sigmoid <- 1 / (1 + exp(-x))
    B <- B_initial + (B_final - B_initial) * sigmoid
  }
  
  # Ensure biomass stays within reasonable bounds
  B <- pmax(B, K * 0.05)  # At least 5% of K
  B <- pmin(B, K * 0.95)  # At most 95% of K
  
  return(B)
}


#' Get Scenario-Specific Parameters
#'
#' @param scenario Stock status scenario
#' @param K Carrying capacity
#' @param Bmsy Biomass at MSY
#'
#' @return List with scenario parameters
get_scenario_params <- function(scenario, K, Bmsy) {
  
  params <- switch(scenario,
    "healthy" = list(
      B_initial = Bmsy * 1.3,      # Start above Bmsy (healthy)
      B_final = Bmsy * 1.1,        # End slightly above Bmsy (slight decline from fishing)
      trajectory_type = "smooth",
      base_effort = 3000,
      description = "Stock declining slightly from fishing but remaining healthy"
    ),
    "overfished" = list(
      B_initial = Bmsy * 0.8,      # Start below Bmsy
      B_final = Bmsy * 0.35,       # End well below Bmsy (overfished!)
      trajectory_type = "smooth",
      base_effort = 6000,
      description = "Stock declining due to overfishing"
    ),
    "recovering" = list(
      B_initial = Bmsy * 0.30,     # Start overfished
      B_final = Bmsy * 0.95,       # End near Bmsy (recovering!)
      trajectory_type = "smooth",
      base_effort = 2000,
      description = "Stock recovering from overfished state"
    ),
    "declining" = list(
      B_initial = Bmsy * 1.3,      # Start healthy
      B_final = Bmsy * 0.60,       # End below Bmsy (declining!)
      trajectory_type = "smooth",
      base_effort = 5000,
      description = "Stock declining from healthy to overfished"
    ),
    "stable" = list(
      B_initial = Bmsy * 1.0,      # Start at Bmsy
      B_final = Bmsy * 1.05,       # End at Bmsy (stable!)
      trajectory_type = "fluctuating",
      base_effort = 4000,
      description = "Stock fluctuating around Bmsy"
    ),
    # Default to healthy
    list(
      B_initial = Bmsy * 0.8,
      B_final = Bmsy * 1.2,
      trajectory_type = "smooth",
      base_effort = 3000,
      description = "Stock at healthy levels"
    )
  )
  
  return(params)
}

#' Generate Data for Multiple Groups
#'
#' Creates historical data for multiple fishing groups with different scenarios
#'
#' @param n_groups Number of groups
#' @param scenarios Vector of scenarios (recycled if needed)
#' @param ... Additional arguments passed to generate_historical_data
#'
#' @return List of data frames, one per group
generate_multi_group_data <- function(n_groups = 4,
                                       scenarios = c("healthy", "overfished", "recovering", "declining"),
                                       ...) {
  
  # Recycle scenarios if needed
  scenarios <- rep(scenarios, length.out = n_groups)
  
  # Generate data for each group
  group_data <- lapply(1:n_groups, function(i) {
    data <- generate_historical_data(scenario = scenarios[i], seed = 100 + i, ...)
    data$Group <- paste0("Group_", LETTERS[i])
    return(data)
  })
  
  names(group_data) <- paste0("Group_", LETTERS[1:n_groups])
  
  return(group_data)
}

#' Plot Catch Data
#'
#' @param data Historical data frame from generate_historical_data
#'
#' @return ggplot object
plot_catch_data <- function(data) {
  
  ggplot(data, aes(x = Date, y = Count)) +
    geom_col(fill = "#2E86AB") +
    labs(
      title = paste0("Historical Catch Data (", n_distinct(data$Date), " years)"),
      x = "Date",
      y = "Count"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold")
    )
}

#' Plot CPUE Time Series
#'
#' @param data Historical data frame
#'
#' @return ggplot object
plot_cpue <- function(data) {
  
  scenario <- attr(data, "scenario")
  final_status <- attr(data, "final_B_Bmsy")

  if(!"CPUE" %in% colnames(data)){
    data$CPUE <- data$Count/data$Effort
  }
  
  ggplot(data, aes(x = as.numeric(Date), y = CPUE)) +
    geom_line(color = "#457B9D", linewidth = 1) +
    geom_point(fill = "#A8DADC", color = "#457B9D", shape = 21, size = 3) +
    #geom_smooth(method = "loess", se = TRUE, color = "#457B9D", fill = "#A8DADC") +
    labs(
      title = "Catch Per Unit Effort (CPUE) Over Time",
      subtitle = sprintf("Scenario: %s | Final B/Bmsy = %.2f", scenario, final_status),
      y = "CPUE (Catch / Effort)",
      x = "Year"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"))
}

#' Plot Biomass Trajectory with Status
#'
#' @param data Historical data frame
#'
#' @return ggplot object
plot_biomass_status <- function(data) {
  
  scenario <- attr(data, "scenario")
  Bmsy <- attr(data, "Bmsy")
  K <- attr(data, "K")
  final_status <- attr(data, "final_B_Bmsy")
  
  # Create status zones
  status_color <- ifelse(data$Biomass > Bmsy, "Healthy", "Overfished")
  
  ggplot(data, aes(x = Date, y = Biomass / Bmsy)) +
    geom_hline(yintercept = 1.0, linetype = "solid", color = "darkgreen", linewidth = 1) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "orange", linewidth = 0.8) +
    geom_ribbon(aes(ymin = 0, ymax = pmin(Biomass/Bmsy, 0.5)), fill = "red", alpha = 0.2) +
    geom_ribbon(aes(ymin = 0.5, ymax = pmin(Biomass/Bmsy, 1.0)), fill = "orange", alpha = 0.2) +
    geom_ribbon(aes(ymin = 1.0, ymax = Biomass/Bmsy), fill = "green", alpha = 0.2) +
    geom_line(linewidth = 1.2, color = "#1d3557") +
    geom_point(size = 3, color = "#1d3557") +
    annotate("text", x = min(data$Date), y = 1.0, label = "Bmsy", 
             vjust = -0.5, hjust = 0, color = "darkgreen", fontface = "bold") +
    annotate("text", x = min(data$Date), y = 0.5, label = "Critical", 
             vjust = 1.5, hjust = 0, color = "orange", fontface = "bold") +
    labs(
      title = "Stock Status Over Time (B/Bmsy)",
      subtitle = sprintf("Scenario: %s | Final status = %.2f", scenario, final_status),
      y = "B / Bmsy",
      caption = "Green = Healthy | Orange = Overfished | Red = Critically Low"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"))
}

#' Check Data Quality Before Fitting
#'
#' Diagnoses potential issues with input data
#'
#' @param data Data frame with Year, Catch, Effort
#'
#' @return List of diagnostic results
check_data_quality <- function(data) {
  
  issues <- character(0)
  warnings <- character(0)
  
  # Check for missing values
  if (any(is.na(data$Count))) {
    issues <- c(issues, "Missing catch values detected")
  }
  if (any(is.na(data$Effort))) {
    issues <- c(issues, "Missing effort values detected")
  }
  
  # Check for zeros
  if (any(data$Count == 0)) {
    warnings <- c(warnings, sprintf("%d years with zero catch", sum(data$Count == 0)))
  }
  if (any(data$Effort == 0)) {
    issues <- c(issues, "Zero effort values detected (will cause CPUE calculation errors)")
  }
  
  # Check for negative values
  if (any(data$Count < 0)) {
    issues <- c(issues, "Negative catch values detected")
  }
  if (any(data$Effort < 0)) {
    issues <- c(issues, "Negative effort values detected")
  }
  
  # Check data length
  if (nrow(data) < 10) {
    warnings <- c(warnings, sprintf("Only %d years of data - model may be poorly constrained", nrow(data)))
  }
  
  # Check CPUE trend
  cpue <- data$Count / data$Effort
  
  # Safely check for trend
  cpue_trend_result <- tryCatch({
    cpue_lm <- lm(cpue ~ data$Date)
    cpue_summary <- summary(cpue_lm)
    list(
      coefficient = coef(cpue_lm)[2],
      p_value = cpue_summary$coefficients[2, 4]
    )
  }, error = function(e) {
    list(coefficient = NA, p_value = NA)
  })
  
  if (!is.na(cpue_trend_result$p_value) && cpue_trend_result$p_value < 0.05) {
    trend_dir <- ifelse(cpue_trend_result$coefficient > 0, "increasing", "decreasing")
    warnings <- c(warnings, sprintf("Significant CPUE trend detected (%s)", trend_dir))
  }
  
  # Check for extreme values
  catch_cv <- sd(data$Count) / mean(data$Count)
  effort_cv <- sd(data$Effort) / mean(data$Effort)
  
  if (catch_cv > 1.0) {
    warnings <- c(warnings, "Very high variability in catch (CV > 100%)")
  }
  if (effort_cv > 1.0) {
    warnings <- c(warnings, "Very high variability in effort (CV > 100%)")
  }
  
  # Summary
  cat("\n=== DATA QUALITY CHECK ===\n")
  cat(sprintf("Years of data: %d\n", nrow(data)))
  cat(sprintf("Catch range: %.0f - %.0f (mean: %.0f)\n", 
              min(data$Count), max(data$Count), mean(data$Count)))
  cat(sprintf("Effort range: %.0f - %.0f (mean: %.0f)\n", 
              min(data$Effort), max(data$Effort), mean(data$Effort)))
  cat(sprintf("CPUE range: %.4f - %.4f (mean: %.4f)\n", 
              min(cpue), max(cpue), mean(cpue)))
  
  if (length(warnings) > 0) {
    cat("\nWARNINGS:\n")
    for (w in warnings) cat("  ", clisymbols::symbol$warning, w, "\n")
  }
  
  if (length(issues) > 0) {
    cat("\nISSUES (may prevent fitting):\n")
    for (i in issues) cat("  ", clisymbols::symbol$cross, i, "\n")
  }
  
  if (length(issues) == 0 && length(warnings) == 0) {
    cat("\n", clisymbols::symbol$tick, " Data quality looks good!\n")
  }
  
  cat("\n")
  
  return(list(
    issues = issues,
    warnings = warnings,
    n_years = nrow(data),
    has_issues = length(issues) > 0
  ))
}


### Scaling functions ##############################

# Participant Catch and Effort Scaling Functions
# Converts workshop marble/candy fishing data to match historical data scale

#' Process Workshop Data with Simple Scaling
#'
#' Takes raw workshop marble counts and scales them appropriately
#'
#' @param workshop_raw Data frame with Year and marble counts (Catch column)
#' @param historical_data Historical data to match
#'
#' @return Data frame with scaled Catch and Effort
process_workshop_simple <- function(workshop_raw, historical_data) {
  
  n_workshop <- nrow(workshop_raw)
  results <- data.frame(
    Year = workshop_raw$Date,
    Raw_Marbles = workshop_raw$Catch,
    Catch = numeric(n_workshop),
    Effort = numeric(n_workshop),
    CPUE = numeric(n_workshop)
  )
  
  for (i in 1:n_workshop) {
    scaled <- simple_scale_workshop_data(
      marble_count = workshop_raw$Catch[i],
      historical_data = historical_data,
      target_cpue_percentile = 0.5
    )
    
    results$Catch[i] <- scaled$catch
    results$Effort[i] <- scaled$effort
    results$CPUE[i] <- scaled$cpue
  }
  
  cat("=== Workshop Data Scaling Summary ===\n")
  cat("\nRaw workshop data:\n")
  print(workshop_raw)
  cat("\nScaled workshop data:\n")
  print(results[, c("Year", "Raw_Marbles", "Catch", "Effort", "CPUE")])
  
  cat("\nHistorical comparison:\n")
  cat(sprintf("  Historical Catch mean: %.0f | Workshop Catch mean: %.0f\n",
              mean(historical_data$Catch), mean(results$Catch)))
  cat(sprintf("  Historical Effort mean: %.0f | Workshop Effort mean: %.0f\n",
              mean(historical_data$Effort), mean(results$Effort)))
  cat(sprintf("  Historical CPUE mean: %.4f | Workshop CPUE mean: %.4f\n",
              mean(historical_data$Catch / historical_data$Effort), mean(results$CPUE)))
  
  return(results)
}

#' Simple Direct Scaling for Workshop Data
#'
#' Scales workshop data to match historical data's scale and CPUE relationship
#' More reliable than the complex estimation approach
#'
#' @param marble_count Number of marbles caught
#' @param time_seconds Time spent fishing (or use 1 for standardized)
#' @param historical_data Historical data to match
#' @param target_cpue_percentile Where in historical CPUE distribution (default: 0.5 = median)
#'
#' @return List with scaled catch and effort
simple_scale_workshop_data <- function(marble_count,
                                       time_seconds = 30,
                                       historical_data,
                                       target_cpue_percentile = 0.5) {
  
  # Calculate historical statistics
  hist_catch_mean <- mean(historical_data$Catch)
  hist_effort_mean <- mean(historical_data$Effort)
  hist_cpue <- historical_data$Catch / historical_data$Effort
  hist_cpue_target <- quantile(hist_cpue, target_cpue_percentile)
  
  # Simple approach: 
  # 1. Scale catch proportional to marbles caught
  # 2. Calculate effort to maintain realistic CPUE
  
  # Assume moderate marble catch (15-20) should give mean catch
  reference_marbles <- 20
  catch_scale <- hist_catch_mean / reference_marbles
  
  scaled_catch <- marble_count * catch_scale
  
  # Calculate effort to achieve target CPUE
  # CPUE = Catch / Effort, so Effort = Catch / CPUE
  scaled_effort <- scaled_catch / hist_cpue_target
  
  # Add some noise for realism (small)
  scaled_catch <- scaled_catch * rlnorm(1, 0, 0.08)
  scaled_effort <- scaled_effort * rlnorm(1, 0, 0.08)
  
  return(list(
    catch = round(scaled_catch, 1),
    effort = round(scaled_effort, 1),
    cpue = round(scaled_catch / scaled_effort, 4)
  ))
}
#'
#' Determines appropriate scaling to convert marble counts and fishing time
#' to match the scale of historical catch and effort data
#'
#' @param historical_data Data frame from generate_historical_data()
#' @param target_catch_range Target range for scaled catch (uses historical mean ± SD if NULL)
#' @param target_effort_range Target range for scaled effort (uses historical mean ± SD if NULL)
#' @param marble_pool_size Total number of target species marbles in the container
#' @param workshop_duration_sec Duration of each fishing event in seconds (default: 30)
#'
#' @return List containing scaling factors and targets
calculate_scaling_factors <- function(historical_data,
                                      target_catch_range = NULL,
                                      target_effort_range = NULL,
                                      marble_pool_size = 150,
                                      workshop_duration_sec = 30) {
  
  # Calculate historical statistics
  hist_catch_mean <- mean(historical_data$Catch)
  hist_catch_sd <- sd(historical_data$Catch)
  hist_effort_mean <- mean(historical_data$Effort)
  hist_effort_sd <- sd(historical_data$Effort)
  
  # Set target ranges if not provided
  if (is.null(target_catch_range)) {
    target_catch_range <- c(
      max(hist_catch_mean - hist_catch_sd, min(historical_data$Catch) * 0.8),
      hist_catch_mean + hist_catch_sd
    )
  }
  
  if (is.null(target_effort_range)) {
    target_effort_range <- c(
      max(hist_effort_mean - hist_effort_sd, min(historical_data$Effort) * 0.8),
      hist_effort_mean + hist_effort_sd
    )
  }
  
  # Estimate expected marble catch range (conservative: 5-30% of pool per 30 sec)
  expected_marble_min <- marble_pool_size * 0.05
  expected_marble_max <- marble_pool_size * 0.30
  
  # Calculate catch scaling factor
  # Maps marble range to historical catch range
  catch_scale <- mean(target_catch_range) / mean(c(expected_marble_min, expected_marble_max))
  
  # Calculate effort scaling factor
  # Maps 30-second intervals to historical effort units
  # Assume participants might do 1-5 "scoops" per 30-second interval
  expected_scoops <- 3  # Average scoops per interval
  
  # IMPORTANT: Scale effort to match historical magnitude
  # Use the ratio of expected standardized units to historical mean
  effort_scale <- hist_effort_mean / expected_scoops
  
  # Create scaling object
  scaling <- list(
    catch_scale = catch_scale,
    effort_scale = effort_scale,
    target_catch_range = target_catch_range,
    target_effort_range = target_effort_range,
    historical_catch_mean = hist_catch_mean,
    historical_effort_mean = hist_effort_mean,
    marble_pool_size = marble_pool_size,
    workshop_duration_sec = workshop_duration_sec,
    timestamp = Sys.time()
  )
  
  class(scaling) <- c("workshop_scaling", "list")
  
  return(scaling)
}

#' Scale Participant Catch Data
#'
#' Converts raw marble counts to historical data scale
#'
#' @param marble_count Number of marbles caught
#' @param scaling Scaling object from calculate_scaling_factors()
#' @param add_noise Logical, add small random variation (default: TRUE)
#' @param noise_cv Coefficient of variation for noise (default: 0.10)
#'
#' @return Scaled catch value
scale_catch <- function(marble_count, 
                        scaling, 
                        add_noise = TRUE, 
                        noise_cv = 0.10) {
  
  # Basic scaling
  scaled_catch <- marble_count * scaling$catch_scale
  
  # Add realistic noise to prevent identical values
  if (add_noise && marble_count > 0) {
    noise_factor <- rlnorm(1, meanlog = 0, sdlog = noise_cv)
    scaled_catch <- scaled_catch * noise_factor
  }
  
  # Ensure non-negative
  scaled_catch <- max(scaled_catch, 0)
  
  return(round(scaled_catch, 1))
}

#' Scale Participant Effort Data
#'
#' Converts fishing time or number of scoops to historical effort scale
#'
#' @param raw_effort Raw effort measure (number of scoops, attempts, or time)
#' @param effort_type Type of effort: "scoops", "time_seconds", or "standardized"
#' @param scaling Scaling object from calculate_scaling_factors()
#' @param add_noise Logical, add small random variation (default: TRUE)
#' @param noise_cv Coefficient of variation for noise (default: 0.10)
#'
#' @return Scaled effort value
scale_effort <- function(raw_effort,
                         effort_type = "standardized",
                         scaling,
                         add_noise = TRUE,
                         noise_cv = 0.10) {
  
  # Standardize effort to common unit if needed
  if (effort_type == "time_seconds") {
    # Convert seconds to standardized units (30 sec = 1 unit)
    standardized_effort <- raw_effort / scaling$workshop_duration_sec
  } else if (effort_type == "scoops") {
    # Already in scoops
    standardized_effort <- raw_effort
  } else {
    # Already standardized (e.g., number of 30-sec intervals)
    standardized_effort <- raw_effort
  }
  
  # Apply scaling
  scaled_effort <- standardized_effort * scaling$effort_scale
  
  # Add realistic noise
  if (add_noise && standardized_effort > 0) {
    noise_factor <- rlnorm(1, meanlog = 0, sdlog = noise_cv)
    scaled_effort <- scaled_effort * noise_factor
  }
  
  # Ensure non-negative
  scaled_effort <- max(scaled_effort, 0.1)
  
  return(round(scaled_effort, 1))
}

#' Process Complete Fishing Event
#'
#' Converts a single fishing event (one "year" in the workshop) to scaled data
#'
#' @param marble_count Number of target species marbles caught
#' @param scoops Number of scoops/attempts made (optional if using time)
#' @param time_seconds Time spent fishing in seconds (optional if using scoops)
#' @param year Year for this observation
#' @param group Group identifier
#' @param scaling Scaling object from calculate_scaling_factors()
#' @param effort_type How to measure effort: "scoops", "time_seconds", or "standardized"
#'
#' @return Data frame with one row of scaled data
process_fishing_event <- function(marble_count,
                                   scoops = NULL,
                                   time_seconds = NULL,
                                   year,
                                   group,
                                   scaling,
                                   effort_type = "standardized") {
  
  # Determine raw effort
  if (effort_type == "scoops" && !is.null(scoops)) {
    raw_effort <- scoops
  } else if (effort_type == "time_seconds" && !is.null(time_seconds)) {
    raw_effort <- time_seconds
  } else {
    # Default: assume standardized (1 unit per 30-second interval)
    raw_effort <- 1
  }
  
  # Scale catch and effort
  scaled_catch <- scale_catch(marble_count, scaling)
  scaled_effort <- scale_effort(raw_effort, effort_type, scaling)
  
  # Calculate CPUE
  cpue <- scaled_catch / scaled_effort
  
  # Create data frame
  result <- data.frame(
    Year = year,
    Group = group,
    Raw_Marbles = marble_count,
    Raw_Effort = raw_effort,
    Catch = scaled_catch,
    Effort = scaled_effort,
    CPUE = round(cpue, 3),
    Timestamp = Sys.time()
  )
  
  return(result)
}

#' Combine Historical and Workshop Data
#'
#' Merges historical data with newly collected workshop data
#'
#' @param historical_data Data frame from generate_historical_data()
#' @param workshop_data Data frame with scaled workshop observations
#' @param group Group identifier to filter/add
#'
#' @return Combined data frame
combine_data <- function(historical_data, workshop_data, group = NULL) {
  
  # Add Group column to historical data if not present
  if (!"Group" %in% names(historical_data)) {
    if (is.null(group)) {
      stop("Must specify group identifier")
    }
    historical_data$Group <- group
  }
  
  # Select relevant columns from workshop data
  workshop_subset <- workshop_data %>%
    select(Year, Group, Catch, Effort, CPUE)
  
  # Add Biomass column to workshop data (NA since unknown)
  workshop_subset$Biomass <- NA
  
  # Select matching columns from historical
  historical_subset <- historical_data %>%
    select(Year, Group, Biomass, Catch, Effort, CPUE)
  
  # Combine
  combined <- rbind(historical_subset, workshop_subset)
  
  # Sort by year
  combined <- combined %>%
    arrange(Year)
  
  return(combined)
}

#' Validate Scaled Data
#'
#' Checks if scaled workshop data is reasonable compared to historical data
#'
#' @param scaled_catch Scaled catch value
#' @param scaled_effort Scaled effort value
#' @param historical_data Historical data frame
#' @param tolerance_factor How many SDs away from mean is acceptable (default: 3)
#'
#' @return List with validation results
validate_scaled_data <- function(scaled_catch,
                                 scaled_effort,
                                 historical_data,
                                 tolerance_factor = 3) {
  
  # Calculate historical ranges
  catch_mean <- mean(historical_data$Catch)
  catch_sd <- sd(historical_data$Catch)
  effort_mean <- mean(historical_data$Effort)
  effort_sd <- sd(historical_data$Effort)
  
  # Check if values are within reasonable range
  catch_ok <- (scaled_catch >= catch_mean - tolerance_factor * catch_sd) &&
              (scaled_catch <= catch_mean + tolerance_factor * catch_sd)
  
  effort_ok <- (scaled_effort >= effort_mean - tolerance_factor * effort_sd) &&
               (scaled_effort <= effort_mean + tolerance_factor * effort_sd)
  
  # Calculate CPUE and check
  cpue <- scaled_catch / scaled_effort
  cpue_mean <- mean(historical_data$CPUE)
  cpue_sd <- sd(historical_data$CPUE)
  
  cpue_ok <- (cpue >= cpue_mean - tolerance_factor * cpue_sd) &&
             (cpue <= cpue_mean + tolerance_factor * cpue_sd)
  
  # Create validation result
  validation <- list(
    catch_valid = catch_ok,
    effort_valid = effort_ok,
    cpue_valid = cpue_ok,
    all_valid = catch_ok && effort_ok && cpue_ok,
    catch_z_score = (scaled_catch - catch_mean) / catch_sd,
    effort_z_score = (scaled_effort - effort_mean) / effort_sd,
    cpue_z_score = (cpue - cpue_mean) / cpue_sd,
    warnings = c()
  )
  
  # Add warnings if needed
  if (!catch_ok) {
    validation$warnings <- c(validation$warnings, 
                            "Catch value outside expected range")
  }
  if (!effort_ok) {
    validation$warnings <- c(validation$warnings,
                            "Effort value outside expected range")
  }
  if (!cpue_ok) {
    validation$warnings <- c(validation$warnings,
                            "CPUE value outside expected range")
  }
  
  return(validation)
}

#' Print Scaling Summary
#'
#' Displays scaling factors and guidance for workshop facilitators
#'
#' @param scaling Scaling object
print.workshop_scaling <- function(scaling) {
  cat("=== Workshop Data Scaling Parameters ===\n\n")
  cat(sprintf("Marble pool size: %d marbles\n", scaling$marble_pool_size))
  cat(sprintf("Fishing duration: %d seconds per event\n", scaling$workshop_duration_sec))
  cat("\nScaling Factors:\n")
  cat(sprintf("  Catch: 1 marble = %.2f catch units\n", scaling$catch_scale))
  cat(sprintf("  Effort: 1 scoop = %.2f effort units\n", scaling$effort_scale))
  cat("\nExpected Ranges (after scaling):\n")
  cat(sprintf("  Catch: %.1f - %.1f\n", 
              scaling$target_catch_range[1], 
              scaling$target_catch_range[2]))
  cat(sprintf("  Effort: %.1f - %.1f\n",
              scaling$target_effort_range[1],
              scaling$target_effort_range[2]))
  cat("\nHistorical Averages:\n")
  cat(sprintf("  Mean Catch: %.1f\n", scaling$historical_catch_mean))
  cat(sprintf("  Mean Effort: %.1f\n", scaling$historical_effort_mean))
  cat("\n")
}

#' Reverse Scale Historical Data to Marble Units
#'
#' Converts historical catch/effort data to the marble/seconds scale
#' that participants will experience in the workshop
#'
#' @param historical_data Data frame with Catch and Effort columns
#' @param reference_marbles What marble count represents mean historical catch (default: 20)
#' @param reference_time_sec What time represents typical effort (default: 30)
#'
#' @return Data frame with additional columns: Marbles, Seconds
reverse_scale_to_marbles <- function(historical_data,
                                     reference_marbles = 20,
                                     reference_time_sec = 30) {
  
  # Calculate scaling factors (inverse of forward scaling)
  hist_catch_mean <- mean(historical_data$Catch)
  hist_effort_mean <- mean(historical_data$Effort)
  
  # Catch scale: historical catch -> marbles
  # If reference_marbles (20) = hist_catch_mean, then
  # marbles = catch * (reference_marbles / hist_catch_mean)
  marble_scale <- reference_marbles / hist_catch_mean
  
  # Effort scale: historical effort -> seconds
  # Scale so that hist_effort_mean = reference_time_sec
  time_scale <- reference_time_sec / hist_effort_mean
  
  # Apply reverse scaling
  historical_data$Marbles <- round(historical_data$Catch * marble_scale, 1)
  historical_data$Seconds <- round(historical_data$Effort * time_scale, 1)
  
  # Calculate CPUE in marble units (marbles per second)
  historical_data$CPUE_marbles <- round(historical_data$Marbles / historical_data$Seconds, 3)
  
  return(historical_data)
}

#' Create Bidirectional Scaling Object
#'
#' Creates a scaling object that can convert both directions:
#' - Workshop marbles -> Historical catch units (forward)
#' - Historical catch -> Workshop marbles (reverse)
#'
#' @param historical_data Historical data frame
#' @param reference_marbles Marble count that equals mean historical catch (default: 20)
#' @param reference_time_sec Time that equals mean historical effort (default: 30)
#'
#' @return Scaling object with both forward and reverse functions
create_bidirectional_scaling <- function(historical_data,
                                        reference_marbles = 20,
                                        reference_time_sec = 30) {
  
  # Calculate historical statistics
  hist_catch_mean <- mean(historical_data$Count)
  hist_effort_mean <- mean(historical_data$Effort)
  hist_cpue <- historical_data$Count / historical_data$Effort
  hist_cpue_median <- median(hist_cpue)
  
  # Calculate scaling factors
  # Forward: marbles -> catch units
  catch_to_marble_scale <- reference_marbles / hist_catch_mean
  marble_to_catch_scale <- hist_catch_mean / reference_marbles
  
  # Forward: time -> effort units
  time_to_effort_scale <- hist_effort_mean / reference_time_sec
  effort_to_time_scale <- reference_time_sec / hist_effort_mean
  
  scaling <- list(
    # Forward scaling (workshop -> historical)
    marble_to_catch = marble_to_catch_scale,
    time_to_effort = time_to_effort_scale,
    
    # Reverse scaling (historical -> workshop)
    catch_to_marble = catch_to_marble_scale,
    effort_to_time = effort_to_time_scale,
    
    # Reference values
    reference_marbles = reference_marbles,
    reference_time_sec = reference_time_sec,
    hist_catch_mean = hist_catch_mean,
    hist_effort_mean = hist_effort_mean,
    hist_cpue_median = hist_cpue_median,
    
    # Functions for easy use
    to_marbles = function(catch_value) {
      round(catch_value * catch_to_marble_scale, 1)
    },
    
    to_catch = function(marble_count) {
      round(marble_count * marble_to_catch_scale, 1)
    },
    
    to_seconds = function(effort_value) {
      round(effort_value * effort_to_time_scale, 1)
    },
    
    to_effort = function(time_seconds) {
      round(time_seconds * time_to_effort_scale, 1)
    }
  )
  
  class(scaling) <- c("bidirectional_scaling", "list")
  
  return(scaling)
}

#' Print Bidirectional Scaling
#'
#' @param scaling Bidirectional scaling object
print.bidirectional_scaling <- function(scaling) {
  cat("=== Bidirectional Scaling Parameters ===\n\n")
  
  cat("Reference Values:\n")
  cat(sprintf("  %d marbles = %.0f catch units (historical mean)\n", 
              scaling$reference_marbles, scaling$hist_catch_mean))
  cat(sprintf("  %d seconds = %.0f effort units (historical mean)\n",
              scaling$reference_time_sec, scaling$hist_effort_mean))
  
  cat("\nScaling Factors:\n")
  cat(sprintf("  1 marble = %.2f catch units\n", scaling$marble_to_catch))
  cat(sprintf("  1 catch unit = %.3f marbles\n", scaling$catch_to_marble))
  cat(sprintf("  1 second = %.2f effort units\n", scaling$time_to_effort))
  cat(sprintf("  1 effort unit = %.3f seconds\n", scaling$effort_to_time))
  
  cat("\nExample Conversions:\n")
  cat(sprintf("  15 marbles -> %.0f catch units\n", scaling$to_catch(15)))
  cat(sprintf("  %.0f catch -> %.1f marbles\n", 
              scaling$hist_catch_mean, scaling$to_marbles(scaling$hist_catch_mean)))
  cat(sprintf("  30 seconds -> %.0f effort units\n", scaling$to_effort(30)))
  cat(sprintf("  %.0f effort -> %.1f seconds\n",
              scaling$hist_effort_mean, scaling$to_seconds(scaling$hist_effort_mean)))
  
  cat("\n")
}

#' Display Historical Data in Workshop Units
#'
#' Convenience function to show historical data in marble/second scale
#' for workshop participants
#'
#' @param historical_data Historical data frame
#' @param scaling Bidirectional scaling object
#' @param columns_to_show Which columns to display (default: Year, Marbles, Seconds)
#'
#' @return Data frame with workshop-scale values
display_for_workshop <- function(historical_data, 
                                 scaling,
                                 columns_to_show = c("Date", "Count", "Effort", "CPUE")) {
  
  # Convert to marble units
  workshop_view <- historical_data
  workshop_view$Count <- scaling$to_marbles(historical_data$Count)
  workshop_view$Effort <- scaling$to_seconds(historical_data$Effort)
  workshop_view$CPUE <- round(workshop_view$Count / workshop_view$Effort, 3)
  
  # Select columns
  result <- workshop_view[, columns_to_show, drop = FALSE]
  
  return(result)
}

#' Convert Workshop Input to Historical Scale
#'
#' Takes participant marble counts and converts to historical scale
#' for model fitting
#'
#' @param workshop_data Data frame with Marbles and Seconds (or just Marbles)
#' @param scaling Bidirectional scaling object
#' @param standard_time If Seconds column missing, use this (default: 30)
#'
#' @return Data frame with Catch and Effort in historical scale
convert_workshop_to_historical <- function(workshop_data,
                                          scaling,
                                          standard_time = 30) {
  
  result <- workshop_data
  
  # Convert marbles to catch
  if ("Marbles" %in% names(workshop_data)) {
    result$Catch <- scaling$to_catch(workshop_data$Marbles)
  }
  
  # Convert seconds to effort
  if ("Seconds" %in% names(workshop_data)) {
    result$Effort <- scaling$to_effort(workshop_data$Seconds)
  } else {
    # Use standard time if not provided
    result$Effort <- scaling$to_effort(standard_time)
  }
  
  # Calculate CPUE
  result$CPUE <- round(result$Catch / result$Effort, 4)
  
  return(result)
}

#' Calculate Realistic Catch Range for Workshop
#'
#' Determines appropriate catch levels based on terminal stock status
#' Ensures workshop catches maintain biological realism
#'
#' @param historical_data Historical data from generate_historical_data()
#' @param n_recent_years Number of recent years to consider (default: 3)
#'
#' @return List with catch range and guidance
calculate_realistic_catch_range <- function(historical_data, 
                                           n_recent_years = 3) {
  
  # Get attributes from historical data
  scenario <- attr(historical_data, "scenario")
  Bmsy <- attr(historical_data, "Bmsy")
  MSY <- attr(historical_data, "MSY")
  final_B_Bmsy <- attr(historical_data, "final_B_Bmsy")
  K <- attr(historical_data, "K")
  r <- attr(historical_data, "r")
  
  # Get recent catch statistics
  recent_data <- tail(historical_data, n_recent_years)
  recent_catch_mean <- mean(recent_data$Catch)
  recent_catch_sd <- sd(recent_data$Catch)
  final_biomass <- tail(historical_data$Biomass, 1)
  
  # Determine sustainable catch based on stock status
  if (final_B_Bmsy < 0.5) {
    # Critically overfished - catch should be well below recent levels
    status <- "Critically Overfished"
    sustainable_catch <- MSY * 0.4  # 40% of MSY
    catch_multiplier <- c(0.3, 0.6)  # 30-60% of recent mean
    
  } else if (final_B_Bmsy < 1.0) {
    # Overfished - catch should be below MSY
    status <- "Overfished"
    sustainable_catch <- MSY * 0.7  # 70% of MSY
    catch_multiplier <- c(0.5, 0.8)  # 50-80% of recent mean
    
  } else if (final_B_Bmsy < 1.3) {
    # Healthy - catch around MSY
    status <- "Healthy"
    sustainable_catch <- MSY
    catch_multiplier <- c(0.7, 1.1)  # 70-110% of recent mean
    
  } else {
    # Very healthy - could sustain higher catch
    status <- "Very Healthy"
    sustainable_catch <- MSY * 1.1
    catch_multiplier <- c(0.8, 1.3)  # 80-130% of recent mean
  }
  
  # Calculate recommended catch range (use more conservative estimate)
  method1_range <- recent_catch_mean * catch_multiplier
  method2_range <- c(sustainable_catch * 0.8, sustainable_catch * 1.2)
  
  # Use the more conservative (lower) range
  catch_range <- c(
    min(method1_range[1], method2_range[1]),
    min(method1_range[2], method2_range[2])
  )
  
  # Also calculate what this means in marbles
  # Inverse of the scaling: if catch_scale = hist_catch_mean / 20
  # then marbles = catch / catch_scale = catch / (hist_catch_mean / 20)
  reference_marbles <- 20
  marble_scale <- mean(historical_data$Catch) / reference_marbles
  
  marble_range <- catch_range / marble_scale
  
  # Create guidance
  guidance <- list(
    stock_status = status,
    final_B_Bmsy = round(final_B_Bmsy, 2),
    final_biomass = round(final_biomass, 0),
    MSY = round(MSY, 0),
    
    # Catch recommendations
    sustainable_catch = round(sustainable_catch, 0),
    catch_range = round(catch_range, 0),
    recent_catch_mean = round(recent_catch_mean, 0),
    
    # Marble pool guidance
    marble_range = round(marble_range, 0),
    recommended_marbles_mean = round(mean(marble_range), 0),
    
    # For setting up marble pools each year
    marble_pools_by_year = calculate_marble_pool_sizes(
      historical_data, 
      n_workshop_years = 3
    )
  )
  
  class(guidance) <- c("catch_guidance", "list")
  return(guidance)
}

#' Calculate Marble Pool Sizes for Each Workshop Year
#'
#' Projects biomass forward and determines appropriate marble counts
#'
#' @param historical_data Historical data
#' @param n_workshop_years Number of workshop fishing years
#'
#' @return Data frame with recommended marble pool sizes
calculate_marble_pool_sizes <- function(historical_data, 
                                       n_workshop_years = 3) {
  
  K <- attr(historical_data, "K")
  r <- attr(historical_data, "r")
  Bmsy <- attr(historical_data, "Bmsy")
  
  final_biomass <- tail(historical_data$Biomass, 1)
  final_catch <- tail(historical_data$Catch, 1)
  
  # Project biomass forward assuming continued fishing at recent levels
  B <- numeric(n_workshop_years + 1)
  B[1] <- final_biomass
  
  for (i in 1:n_workshop_years) {
    surplus <- r * B[i] * (1 - B[i] / K)
    # Assume catch continues at ~70% of final historical catch
    projected_catch <- final_catch * 0.7
    B[i + 1] <- B[i] + surplus - projected_catch
    B[i + 1] <- max(B[i + 1], K * 0.05)
  }
  
  # Convert biomass to marble counts
  # Scale so that Bmsy = 100 marbles (reasonable pool size)
  marbles_at_Bmsy <- 100
  marble_scale <- marbles_at_Bmsy / Bmsy
  
  pool_sizes <- data.frame(
    Year = 1:n_workshop_years,
    Projected_Biomass = round(B[2:(n_workshop_years + 1)], 0),
    B_Bmsy = round(B[2:(n_workshop_years + 1)] / Bmsy, 2),
    Marble_Pool_Size = round(B[2:(n_workshop_years + 1)] * marble_scale, 0),
    Expected_Catch_Marbles = round(pmin(
      B[2:(n_workshop_years + 1)] * marble_scale * 0.2,  # Max 20% of pool
      30  # Cap at 30 marbles
    ), 0)
  )
  
  return(pool_sizes)
}

#' Print Catch Guidance
#'
#' @param guidance Catch guidance object
print.catch_guidance <- function(guidance) {
  
  cat("=== WORKSHOP CATCH GUIDANCE ===\n\n")
  
  cat("STOCK STATUS (end of historical period):\n")
  cat(sprintf("  Status: %s\n", guidance$stock_status))
  cat(sprintf("  Final B/Bmsy: %.2f %s\n", 
              guidance$final_B_Bmsy,
              ifelse(guidance$final_B_Bmsy > 1, clisymbols::symbol$tick, clisymbols::symbol$cross)))
  cat(sprintf("  Final Biomass: %.0f\n", guidance$final_biomass))
  cat(sprintf("  MSY: %.0f\n\n", guidance$MSY))
  
  cat("RECOMMENDED CATCH LEVELS (scaled units):\n")
  cat(sprintf("  Sustainable catch target: %.0f\n", guidance$sustainable_catch))
  cat(sprintf("  Recommended range: %.0f - %.0f\n", 
              guidance$catch_range[1], guidance$catch_range[2]))
  cat(sprintf("  Recent historical mean: %.0f\n\n", guidance$recent_catch_mean))
  
  cat("MARBLE COUNT GUIDANCE:\n")
  cat(sprintf("  Expected marble catch range: %d - %d marbles\n",
              guidance$marble_range[1], guidance$marble_range[2]))
  cat(sprintf("  Target mean: ~%d marbles per fishing event\n\n",
              guidance$recommended_marbles_mean))
  
  cat("MARBLE POOL SETUP (by workshop year):\n")
  print(guidance$marble_pools_by_year)
  
  cat("\nINTERPRETATION:\n")
  if (guidance$stock_status == "Critically Overfished") {
    cat("  The stock is in critical condition. Participants should catch\n")
    cat("  LESS than recent historical levels to allow recovery.\n")
    cat("  Consider reducing marble pool size each year to simulate depletion.\n")
  } else if (guidance$stock_status == "Overfished") {
    cat("  The stock is below target levels. Participants should practice\n")
    cat("  restraint and catch less than historical averages.\n")
  } else if (guidance$stock_status == "Healthy") {
    cat("  The stock is at sustainable levels. Participants can maintain\n")
    cat("  catches similar to recent years.\n")
  } else {
    cat("  The stock is very healthy. Participants could increase catches\n")
    cat("  slightly while remaining sustainable.\n")
  }
  
  cat("\n")
}
