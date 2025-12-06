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
  # F_pattern = fishing mortality pattern
  # sigma_proc = process error SD
  # sigma_obs = observation error SD
simulate_population <- function(years = 30, K, r, q, B0_frac = 0.9, 
                                F_pattern = "increasing", sigma_proc = 0.1, 
                                sigma_obs = 0.15) {
  
  B <- numeric(years)
  C <- numeric(years)
  CPUE <- numeric(years)
  F_vec <- numeric(years)
  
  B[1] <- K * B0_frac
  
  # Define fishing mortality pattern
  if (F_pattern == "increasing") {
    F_vec <- seq(0.05, 0.5, length.out = years)
  } else if (F_pattern == "constant") {
    F_vec <- rep(0.3, years)
  } else if (F_pattern == "variable") {
    F_vec <- 0.2 + 0.15 * sin(seq(0, 4*pi, length.out = years))
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

pop <- simulate_population(years = 15, K = 1000, r = .2, 
q = .0001, B0_frac = 0.9, F_pattern = "variable")

ggplot(data = pop) +
geom_line(aes(x = Year, y = Biomass))

plot(x = pop$Year, y = pop$Catch, type = "l")
plot(x = pop$Year, y = pop$CPUE, type = "l")


pop <- simulate_population(years = 15, K = 1000, r = .6, 
q = .0001, B0_frac = 0.9, F_pattern = "variable")

ggplot(data = pop) +
geom_line(aes(x = Year, y = Biomass))

plot(x = pop$Year, y = pop$Catch, type = "l")
plot(x = pop$Year, y = pop$CPUE, type = "l")

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
lines(x = biomass_df[-16,1], y = biomass_df[-16,2], lty = 2, color = "red")
legend("topright", legend = c("OM", "fit"), lty = c(1,2))

