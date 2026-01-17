# Code to simulate a population, catch and CPUE data using a surplus production model.
# Simulated data can then be fit with MQMF functions to fit SPM. 
# See Using R for Fisheries Modelling (Haddon 2023) chapter 7 for example code
# https://haddonm.github.io/URMQMF/surplus-production-models.html
library(ggplot2)
library(dplyr)

# Function to simulate population dynamics with catch
  # Parameters:
  # K = carrying capacity
  # r = intrinsic growth rate
  # q = catchability coefficient
  # B0_frac = initial biomass as fraction of K
  # F_pattern = fishing mortality pattern ("increasing", "constant", "variable")
  # sigma_proc = process error SD
  # sigma_obs = observation error SD
simulate_population <- function(years = 30, K, r, q, B0_frac = 0.9, 
                                F_pattern = "increasing", sigma_proc = 0.1, 
                                sigma_obs = 0.15, F_scale = 0.001) {
  
  B <- numeric(years)
  C <- numeric(years)
  CPUE <- numeric(years)
  F_vec <- numeric(years)
  
  B[1] <- K * B0_frac
  
  # Define fishing mortality pattern
  if (F_pattern == "increasing") {
    F_vec <- seq(0.05, 0.5, length.out = years) * F_scale  # Apply scale
  } else if (F_pattern == "constant") {
    F_vec <- rep(0.3, years) * F_scale  # Apply scale
  } else if (F_pattern == "variable") {
    F_vec <- (0.2 + 0.15 * sin(seq(0, 4*pi, length.out = years))) * F_scale  # Apply scale
  }
  
  # Simulate population dynamics
  for (t in 1:(years-1)) {
    # Schaefer surplus production
    surplus <- r * B[t] * (1 - B[t]/K)
    
    # Catch
    C[t] <- F_vec[t] * B[t]
    
    # Process error (lognormal)
    proc_error <- exp(rnorm(1, -sigma_proc^2/2, sigma_proc))
    
    # Next year's biomass
    B[t+1] <- max(0.01 * K, (B[t] + surplus - C[t]) * proc_error)
  }
  
  # Final year catch
  C[years] <- F_vec[years] * B[years]
  
  # Generate CPUE with observation error
  for (t in 1:years) {
    obs_error <- exp(rnorm(1, -sigma_obs^2/2, sigma_obs))
    CPUE[t] <- q * B[t] * obs_error
  }
  
  data.frame(
    Year = 1:years,
    Biomass = B,
    Catch = C,
    CPUE = CPUE,
    F = F_vec
  )
}

# Read in species life history parameter values
species_lh <- read.csv(file.path(getwd(), "R", "species_LH_params.csv"))
# Generate simulated catch and CPUE data
set.seed(456)
scale_factor <- .01
simulated_data <- lapply(1:nrow(species_lh), function(i) {
  data <- simulate_population(
    years = 15,
    K = species_lh$K[i]*scale_factor,
    r = species_lh$r[i],
    q = species_lh$q[i]/scale_factor,
    F_pattern = "increasing",
    sigma_proc = 0.1,
    sigma_obs = 0.15,
    F_scale = scale_factor
  )
  data$Species <- species_lh$SPECIES[i]
  data
})

names(simulated_data) <- species_lh$SPECIES
all_data <- bind_rows(simulated_data) %>%
  mutate(Effort = round((Catch/CPUE)*60),
  Catch = round(Catch),
  CPUE = round(CPUE, 2)) #calculate effort in terms of catch per second


all_data %>%
  group_by(Species) %>%
  summarise(
    Min_Catch = round(min(Catch)),
    Max_Catch = round(max(Catch)),
    Avg_Catch = round(mean(Catch)),
    Min_CPUE = round(min(CPUE), 2),
    Max_CPUE = round(max(CPUE), 2),
    Min_Effort = round(min(Effort)),
    Max_Effort = round(max(Effort))
  )

ggplot(data = all_data, aes(x = Year)) + 
geom_line(aes(y = Catch, group = Species, color = Species), linewidth = 1.2)

ggplot(data = all_data, aes(x = Year)) + 
geom_line(aes(y = CPUE, group = Species, color = Species), linewidth = 1.2)

ggplot(data = all_data, aes(x = Year)) + 
geom_line(aes(y = Effort, group = Species, color = Species), linewidth = 1.2)

ggplot(data = all_data, aes(x = Year)) + 
geom_line(aes(y = Biomass, group = Species, color = Species), linewidth = 1.2)

write.csv(all_data, "./R/historical_data.csv", row.names = FALSE)

hist_catch <- read.csv("./R/historical_data.csv")
pop <- simulate_population(years = 15, K = 1000, r = .2, 
q = .0001, B0_frac = 0.9, F_pattern = "variable")

ggplot(data = pop) +
geom_line(aes(x = Year, y = Biomass))

plot(x = pop$Year, y = pop$Catch, type = "l")
plot(x = pop$Year, y = pop$CPUE, type = "l")


yt_pop <- simulate_population(years = 15, K = species_lh$K[1], r = species_lh$r[1], 
q = species_lh$q[1], B0_frac = 0.9, F_pattern = "variable")

ggplot(data = pop) +
geom_line(aes(x = Year, y = Biomass))

plot(x = pop$Year, y = pop$Catch, type = "l")
plot(x = pop$Year, y = pop$CPUE, type = "l")


# Convert M&M counts to the same scale as simulated CPUE and catch above: 
# Function to convert M&M data to match simulated catch/CPUE
convert_user_to_simulated_format <- function(fish_data, species_params) {
  
  # Create species name mapping (user-friendly to code names)
  species_mapping <- data.frame(
    display_name = c("Yellowfin Tuna", "Mahi Mahi", "Opakapaka", 
                     "Peacock Grouper", "Yellowfin Goatfish"),
    code_name = c("yellowfin_tuna", "mahi_mahi", "deepwater_snapper", 
                  "peacock_grouper", "yellowfin_goatfish"),
    stringsAsFactors = FALSE
  )
  
  # Convert species names to code names
  fish_data_converted <- fish_data %>%
    left_join(species_mapping, by = c("Species" = "display_name")) %>%
    mutate(
      Year = as.numeric(Date),
      Catch = Total_Landings,  # Total_Landings in kg
      CPUE = Total_Landings / Effort,  # kg per unit effort
      Biomass = NA,  # Not observed by user
      F = NA  # Not observed by user
    ) %>%
    select(Year, Biomass, Catch, CPUE, F, Species = code_name) %>%
    arrange(Species, Year)
  
  # Split into separate dataframes by species (matching simulated_data structure)
  user_data_list <- split(fish_data_converted, fish_data_converted$Species)
  
  return(user_data_list)
}

# Example usage:
fish_data_df <- data.frame(
  Species = c("Yellowfin Tuna", "Mahi Mahi", "Opakapaka", "Mahi Mahi", 
              "Peacock Grouper", "Opakapaka", "Yellowfin Tuna", "Mahi Mahi", 
              "Peacock Grouper", "Opakapaka"),
  Date = c("2024", "2024", "2024", "2024", "2024", "2024", "2024", "2024", 
           "2024", "2024"),
  Count = c(5, 8, 3, 12, 4, 2, 6, 7, 9, 3),
  Effort = c(6.5, 7.0, 5.5, 8.0, 6.0, 5.0, 7.5, 6.5, 8.5, 5.5),
  Total_Landings = c(125, 160, 75, 240, 80, 50, 150, 140, 180, 60)
)

# Convert user data
user_data_list <- convert_user_to_simulated_format(fish_data_df, species_lh)

# Combine with simulated data for each species
combined_data_list <- lapply(names(simulated_data), function(sp) {
  if (sp %in% names(user_data_list)) {
    rbind(simulated_data[[sp]], user_data_list[[sp]])
  } else {
    simulated_data[[sp]]  # Keep simulated data if no user data for this species
  }
})
names(combined_data_list) <- names(simulated_data)

# Combine with simulated data
combined_data <- rbind(
  simulated_data[["Yellowfin_tuna"]],
  user_data1
)


# fitting simulated data with Schaefer surplus production model
library(MQMF)
data(dataspm)
head(dataspm)

# put parameter start values in log space
param <- log(c(r=0.2,K=1000,Binit=900,sigma=0.1))  

# put data in proper format for function
fish <- pop %>% 
select(Year, Catch, CPUE) %>% 
rename_with(tolower)

# fit SPM 
ans <- fitSPM(pars=param,fish=fish,schaefer=TRUE,maxiter=1000,  
             funkone=TRUE)  

# fit a second time to ensure estimates are robust 
out <- robustSPM(ans$estimate,fish,N=100,scaler=15,  
                verbose=FALSE,funkone=TRUE) 

results <- plotspmmod(inp=param,indat=fish,schaefer=TRUE,  
                 addrmse=TRUE,plotprod=FALSE) 

# compare OM biomass and fitted biomass estimates
biomass_df <- cbind(results$Dynamics$outmat[,1], results$Dynamics$outmat[,2], pop$Biomass)

plot(x = biomass_df[-16,1], y = biomass_df[-16,3], type = "l")
lines(x = biomass_df[-16,1], y = biomass_df[-16,2], lty = 2, col = "red")
legend("topright", legend = c("OM", "fit"), lty = c(1,2))

