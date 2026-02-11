# Code to simulate catch and CPUE data using a surplus production model. Four terminal stock status scenarios are
# available: healthy, "overfished", "recovering", or "declining". 
# Simulated data can then be combined with data collected during the workshop by participants. 
# Workshop data is rescaled, before combining with the historical data to get CPUE on same scale. 
# Combined data is then fit with a surplus production model and biomass, F, and reference points are estimated. 
# Terminal stock status is evaluated and the particpants can discuss how fishing activities affected the population.  

library(ggplot2)
library(dplyr)
library(purrr)
library(tidyr)

##-------------------- sourcing functions ---------------------#

source("./R/sim_data_funs.R") 
source("./R/fit_spm_funs.R")
sps_lh <- read.csv("./R/species_LH_params.csv")

##-------------------- Generate historical + workshop data ---------------------#
## using opaka LH parameters
historical <- generate_historical_data(
  n_years = 12, 
  scenario = "overfished",  # stock will end in overfished status
  K = 300000,
  r = 0.18,
  q = 2.5e-05,
  start_year = 2012,
  seed = 42
)

scenarios_vec <- c("healthy", "overfished", "recovering", "declining")
species_vec <- sps_lh$Species

run_grid <- expand_grid(
  Species = species_vec, 
  scenario = scenarios_vec
) %>%
  left_join(sps_lh, by = "Species")

results <- run_grid %>%
  mutate(historical_data = pmap(list(scenario, K, r, q), function(scen, k_val, r_val, q_val) {
    generate_historical_data(
      n_years = 12,
      scenario = scen,
      K = k_val,
      r = r_val,
      q = q_val,
      start_year = 2012,
      seed = 42
    )
  }))

tuna_healthy <- results %>% 
  filter(Species == "Yellowfin_tuna" & scenario == "healthy") %>%
  pull(historical_data)

final_flat_data <- results %>% unnest(historical_data) %>% rename("Scenario" = scenario)
write.csv(final_flat_data, "./R/historical_data.csv", row.names = FALSE)

final_flat_data %>%
ggplot(aes(x = Year, y = Biomass)) +
geom_line(aes(group = Scenario, color = Scenario)) +
facet_wrap(~Species, scales = "free_y") + 
theme_bw()

##-------------------- Get suggested workshop setup ---------------------#
# calculate realistic catch numbers for the workshop participants based on 
# terminal year catch of the historical data
guidance <- calculate_realistic_catch_range(historical)
# see "MARBLE POOL SETUP" section for advice for setting up the fishing scenarios
print(guidance)

# Create guidance with all necessary info preserved
guidance_all <- final_flat_data %>%
  group_by(Species, Scenario, K, r, q) %>%
  nest() %>%
  mutate(
    # Add attributes that the function expects
    guidance = pmap(list(data, Scenario, K, r, q), function(df, scen, K_val, r_val, q_val) {
      
      Bmsy <- K_val / 2
      MSY <- r_val * K_val / 4
      
      # Add attributes
      attr(df, "K") <- K_val
      attr(df, "r") <- r_val
      attr(df, "q") <- q_val
      attr(df, "Bmsy") <- Bmsy
      attr(df, "MSY") <- MSY
      attr(df, "scenario") <- scen
      attr(df, "final_B_Bmsy") <- tail(df$Biomass, 1) / Bmsy
      
      # Calculate guidance
      calculate_realistic_catch_range(df, n_recent_years = 3)
    })
  )

# View guidance for a specific case
guidance_all %>%
  filter(Species == "Yellowfin_tuna", Scenario == "healthy") %>%
  pull(guidance) %>%
  .[[1]] %>%
  print()

# Create summary table
guidance_summary <- guidance_all %>%
  mutate(
    stock_status = map_chr(guidance, ~ .x$stock_status),
    final_B_Bmsy = map_dbl(guidance, ~ .x$final_B_Bmsy),
    sustainable_catch = map_dbl(guidance, ~ .x$sustainable_catch),
    marble_range_min = map_dbl(guidance, ~ .x$marble_range[1]),
    marble_range_max = map_dbl(guidance, ~ .x$marble_range[2]),
    recommended_marbles = map_dbl(guidance, ~ .x$recommended_marbles_mean)
  ) %>%
  select(Species, Scenario, stock_status, final_B_Bmsy, 
         sustainable_catch, marble_range_min, marble_range_max, 
         recommended_marbles)

print(guidance_summary, n = 25)
write.csv(guidance_summary, file = "./setup_guidance.csv", row.names = FALSE)

##-------------------- Combine historical and workshop data ---------------------#
# Add 3 additional workshop years
workshop_data <- data.frame(
  Year = 2024:2026,
  Catch = c(12,10,8)
)

# properly scale workshop data to historical data
workshop_scaled <- process_workshop_simple(workshop_data, historical)

combined_data <- rbind(
  historical %>% select(Year, Catch, Effort),
  workshop_scaled %>% select(Year, Catch, Effort)
)

##-------------------- Reverse scale historical to workshop data for shiny app ----------------------#

# 1. Create the bidirectional scaling object
hist_catch_shiny_yt <- hist_catch_shiny %>% filter(Species == "Yellowfin_tuna")

scaling <- create_bidirectional_scaling(
  historical_data = hist_catch_shiny_yt,
  reference_marbles = 20,    # 20 marbles = mean historical catch
  reference_time_sec = 30     # 30 seconds = mean historical effort
)
workshop_display <- display_for_workshop(hist_catch_shiny_yt, scaling, columns_to_show = c("Species", "Date", "Count", "Effort"))
print(workshop_display)

combined_data <- workshop_display %>% select(-CPUE_marbles) %>% rename("Year" = Date, "Catch" = Marbles, "Effort" = Seconds)
scaling_by_species_yt <- scaling_by_species %>% filter(Species == "Yellowfin_tuna" & Scenario == "healthy")
##-------------------- Fit model ---------------------#
model_fit <- fit_schaefer_model(
  data = scaling_by_species_yt,
  use_cpue = TRUE
)

##-------------------- Plot results ---------------------#
print(model_fit)

# Create plots
plot_model_fit(model_fit)

plot_biomass_trajectory(model_fit)

plot_kobe(model_fit) 

plot_catch_data(scaling_by_species_yt)

plot_cpue(scaling_by_species_yt)
