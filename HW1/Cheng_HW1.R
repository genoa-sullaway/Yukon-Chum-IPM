# Purpose: To complete HW1 for FISH622
# Creator: Matthew LH. Cheng (UAF-CFOS)
# Date: 2/1/23


# Set up ------------------------------------------------------------------

library(here)
library(bbmle)
library(tidyverse)

# Problem 2 ---------------------------------------------------------------

# load in data
trop_df <- read.csv(here( "HW1", "troptuna.csv"))

# Create function that calculates SSQ between observed and predicted
#'@param year vector of year intervals
#'@param N0 initial unfished biomass
#'@param r initial rate of decline to delta
#' @param delta fraction of pop remaining when at equilibrium
cpue_pred <- function(year, N0, r, delta) {
  
  # Calculate predicted CPUE with modified exponential model
  pred_cpue <- N0 * ((1 - delta) * exp(-r * year) + delta)
  
  return(pred_cpue)
  
} # end function

# Create function called that calculates SSQ
#'@param obs Vector of observations
cpue_ssq <- function(obs, year, N0, r, delta) {
  
  # Get predicted CPUE values
  cpue_pred <- cpue_pred(year = year, N0 = N0 ,
                         r = r, delta = delta)
  
  # Get residual sum of squares
  RSS <- sum((obs - cpue_pred)^2)
  
  return(RSS)
} # end function

# Specify years
year <- 0:(nrow(trop_df)-1)

# Now, fit the model to the data using SSQ
trop_mod_ssq <- mle2(
  minuslogl = cpue_ssq, # our objective function
  start = list(N0 = 12, # Starting values for parameters
               r = 0.1, 
               delta = 0.1),
  data = list(obs = trop_df$cpue, # Specifying data here
              year = year),
  method = "Nelder-Mead")

# Now fit the model to the data using a normal likelihood
#'@param ln_sigma Standard deviation for normal likelihood in log space
cpue_normLike <- function(obs, year, N0, r, delta, ln_sigma) {
  
  # Get predicted CPUE values
  cpue_pred <- cpue_pred(year = year, N0 = N0 ,
                         r = r, delta = delta)
  
  # exponentiate to normal space
  sigma <- exp(ln_sigma)
  
  # Now, evaluate our likelihoods here
  nLL <-  -1 * sum(dnorm(obs, cpue_pred, sd = sigma, log = TRUE))
  
  return(nLL)
} # end function


### fit full dataset --------------------------------------------------------

# Fit model using normal likelihood
trop_mod_dnorm <- mle2(
  minuslogl = cpue_normLike, # obj function
  start = list(N0 = 12, # Starting values for parameters
               r = 0.1, 
               delta = 0.1,
               ln_sigma = 0),
  data = list(obs = trop_df$cpue, # Specifying data here
              year = year),
  method = "Nelder-Mead")

# Explore confidence intervals
confint_dnorm <- confint(trop_mod_dnorm)

# coerce to df
trop_par_est_all <- data.frame(
  par = names(coef(trop_mod_dnorm)),
  mle_val = coef(trop_mod_dnorm),
  lwr_95 = confint_dnorm[,1],
  upr_95 = confint_dnorm[,2],
  data = "all"
)

# sigma in normal space
trop_par_est_all[4,] <- unlist(c("sigma",exp(trop_par_est_all[4,-c(1,5)]), "all"))


# Problem 3 ---------------------------------------------------------------

### w/o first 10 obs --------------------------------------------------------

# Fit model without first 10 obs
trop_mod_10 <- mle2(
  minuslogl = cpue_normLike, # obj function
  start = list(N0 = 10, # Starting values for parameters
               r = 0.1, 
               delta = 0.1,
               ln_sigma = 0),
  data = list(obs = trop_df$cpue[-c(1:10)], # Specifying data here
# presuambly... we're starting at year 10 (as opposed to resetting to 0), just so all models are comparable?
              year = year[-c(1:10)]), 
  control = list(maxit = 1e4), # need to increase number of iterations (also model is super sensitive
  # to starting values of sigma)
  method = "Nelder-Mead")

# Explore confidence intervals
confint_trop_10 <- confint(trop_mod_10)

# coerce to df
trop_par_est_10 <- data.frame(
  par = names(coef(trop_mod_10)),
  mle_val = coef(trop_mod_10),
  lwr_95 = confint_trop_10[,1],
  upr_95 = confint_trop_10[,2],
  data = "w/o first 10 obs"
)

# sigma in normal space
trop_par_est_10[4,] <- unlist(c("sigma",exp(trop_par_est_10[4,-c(1,5)]), "w/o first 10 obs"))


### w/o last 20 obs ---------------------------------------------------------

# Get index for last 20 obs
last20_idx <- (nrow(trop_df) - 20):nrow(trop_df)

trop_mod_20 <- mle2(
  minuslogl = cpue_normLike, # obj function
  start = list(N0 = 10, # Starting values for parameters
               r = 0.1, 
               delta = 0.1,
               ln_sigma = 0),
  data = list(obs = trop_df$cpue[-c(last20_idx)], # Specifying data here
              year = year[-c(last20_idx)]), 
  control = list(maxit = 1e4), 
  method = "Nelder-Mead")

# Explore confidence intervals
confint_trop_20 <- confint(trop_mod_20)

# coerce to df
trop_par_est_20 <- data.frame(
  par = names(coef(trop_mod_20)),
  mle_val = coef(trop_mod_20),
  lwr_95 = confint_trop_20[,1],
  upr_95 = confint_trop_20[,2],
  data = "w/o last 20 obs"
)

# sigma in normal space
trop_par_est_20[4,] <- unlist(c("sigma",exp(trop_par_est_20[4,-c(1,5)]), "w/o last 20 obs"))


### Visualize ---------------------------------------------------------------
# First, bind all of the model results into one dataframe
trop_mod_results <- rbind(trop_par_est_all, trop_par_est_10,trop_par_est_20)

# Coerce to numeric
trop_mod_results <- trop_mod_results %>% 
  mutate(mle_val = as.numeric(mle_val),
         lwr_95 = as.numeric(lwr_95),
         upr_95 = as.numeric(upr_95))

# Now visualize parameter uncertainty
ggplot(trop_mod_results, aes(x = data, y = mle_val, 
                             ymin = lwr_95, ymax = upr_95)) +
  geom_pointrange() +
  facet_wrap(~par, scales = "free") +
  labs(x = "Model", y = "Value") +
  theme_bw()

# Now visualize predictions from the model, given parameter estimates
trop_mod_results_wide <- trop_mod_results %>% pivot_wider(names_from = "par",
                     values_from = c("mle_val", "lwr_95", "upr_95"))


# Empty df to store
pred_all <- data.frame()

# Loop through
for(i in 1:3) {
  
  # Extract parameter estimates
  N0 <- trop_mod_results_wide$mle_val_N0[i]
  r <- trop_mod_results_wide$mle_val_r[i]
  delta <- trop_mod_results_wide$mle_val_delta[i]
  model_name <- trop_mod_results_wide$data[i]
  
  # Make predictions with parameters
  pred_df <- data.frame(year = 0:43,
    pred_cpue = cpue_pred(year = 0:43, N0 = N0, r = r, delta = delta),
    model = model_name, obs = trop_df$cpue
  )
  
  # Bind with empty df
  pred_all <- rbind(pred_df, pred_all)
  
} # end i

# Plot this out!
ggplot(pred_all, aes(x = year, y = pred_cpue, color = model)) +
  geom_line(size = 1.3, alpha = 0.8) +
  geom_point(aes(y = obs), color = "black", size = 2) +
  labs(x = "Year", y = "CPUE")





