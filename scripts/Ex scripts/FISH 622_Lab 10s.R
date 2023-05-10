#==================================================================================================
#Project Name: FISH 622 - QUANTITATIVE FISH POPULATION DYNAMICS: Lab #10 Age-structure Assessments in R
#Creator: Curry James Cunningham, College of Fisheries and Ocean Sciences, UAF
#Date: April 16, 2021
#
#Purpose: 
#
# =========================================================================================
library(tidyverse)
library(dplyr)
library(manipulate)
library(bbmle)
library(visreg) #Install any of these packages if you don't have them already
library(ggthemes)
library(ggridges)
library(reshape2)
library(cowplot)
# Set your working directory!!!!!
#setwd("/Users/curryc2/Documents/Students/2021/622 - Quant Fish Pop Dynamics/Content/Week 10/Lab")


# Part A: Simple Age-Structured Assessment Model in R ===========================================

# Here we will investigating how to develop and implement the simple age-structured assessment model
#   we explored in Lab #9 in a spreadsheet format


# DEFINE DEMOGRAPHIC COMPONENTS
Nyear <- 20 # Number of years
Nage <- 12 # Number of ages

# Define ages
ages <- 0:Nage


Mval <- 0.2 # Assumed natural mortality

# Observation Error Variance Terms
SigCatch <- 0.05 # Standard deviation for catch error
SigCPUE <- 0.2 # Standard deviation for survey index CPUE likelihood

# Weighting parameter for age proportion data
Omega <- 50

# Read in Data files for the stock =================================
# First we will read in the data we have available

# Weight-at-age
waa <- read.csv(file="AS1_weight.csv", header=FALSE)[,1]

# Lets quickly plot expected weight-at-age
plot(x=ages, y=waa, type="l", col="blue", lwd=3,
     xlab="Age", ylab="Weight")
grid(col="black")
points(x=ages, y=waa, pch=21, bg="blue")

# Next, lets load our timeseries of catch (yield) and Survey CPUE
CatchCPUE <- read.csv(file="AS1_catchCPUE.csv", header=TRUE)
str(CatchCPUE)

# Lets plot out the catch and CPUE timeseries
#   First, we can plot catch and survey CPUE as lines,
#     but we need to reshape the CatchCPUE data frame so we have a data column and a values column
CatchCPUE.list <- CatchCPUE %>% pivot_longer(cols=Catch:CPUE, names_to="Type", values_to="value")

ggplot(CatchCPUE.list, aes(x=Year, y=value, color=Type)) +
    scale_color_colorblind() +
    geom_line() +
    geom_point(pch=21) +
    ylab("Survey CPUE or Catch")

# Well that is boring, lets plot our CPUE index as a line with points, and our catch timeseries as
#   columns. 
ggplot(CatchCPUE, aes(x=Year)) +
    geom_line(aes(y=CPUE, color=CPUE), show.legend = TRUE) +
    geom_point(aes(y=CPUE, size=CPUE, color=CPUE)) +
    scale_color_viridis_c() +
    geom_col(aes(y=Catch, fill=Catch), alpha=0.75) +
    scale_fill_viridis_c(option=3) +
    ylab("Survey CPUE or Catch")


# Finally, lets load the observed catch age composition (proportions at age by year)
Propn <- read.csv("AS1_Propn.csv")
Propn

# Lets plot our observed catch age composition proportions
#   But first, we will convert the data frame to a list for easy plotting
Propn.df <- Propn %>% pivot_longer(cols=!Year, names_to="Age", values_to="prop")
Propn.df

# Ok time to plot

# But, first, lets re-order the age factor
Propn.df$Age <- factor(Propn.df$Age, ordered=TRUE, levels=paste0("Age_",c(0:12)))
levels(Propn.df$Age)


ggplot(data=Propn.df, aes(x=Year, y=prop, fill=Age)) +
    theme_linedraw() +
    scale_fill_viridis_d() +
    geom_area()

# Or maybe as columns
ggplot(data=Propn.df, aes(x=Year, y=prop, fill=Age)) +
  theme_linedraw() +
  scale_fill_viridis_d() +
  geom_col()

# Or maybe as a way to look for cohorts
ggplot(data=Propn.df, aes(x=Age, y=prop, fill=Age)) +
  theme_linedraw() +
  scale_fill_viridis_d() +
  geom_col() +
  facet_wrap(~Year)

# Or maybe as points
ggplot(data=Propn.df, aes(x=Year, y=Age, color=prop, size=prop)) +
  theme_linedraw() +
  scale_color_viridis_c() +
  geom_point() 

# Next, lets define a function for asymptotic selectivity in terms of two parameters 
#   1) Sel50 - the age at 50% selectivity
#   2) Sel95 - the age at 95% selectivity

select_fxn <- function(Age, Sel50, Sel95) {
  selex <- 1.0 / (1 + exp(-log(19) * (Age-Sel50) / (Sel95-Sel50)))
  return(selex)
}

# Lets check that it works
select_fxn(Age=ages, Sel50=4, Sel95=10)

# Now, to get a sense of our newly-defined selectivity function
#   lets create a function to plot predictions, given a vector of Age, and values
#     for the Sel95, and Sel50 parameters
plot_select <- function(Age, Sel50, Sel95) {
  # Predict Selectivty
  selex <- select_fxn(Age=Age, Sel50=Sel50, Sel95=Sel95)
  # Plot Selectivity
  data.frame(Age,selex) %>% ggplot(aes(x=Age, y=selex)) +
                              geom_col(aes(fill=selex)) +
                              scale_fill_viridis_c() +
                              geom_line(color="red", lwd=2) +
                              ylab("Selectivity at Age") +
                              theme_wsj() #Change theme to match Walstreet Journal
}



# Now, lets use manipulate to explore selectivity
manipulate(plot_select(Age=ages, Sel50, Sel95), 
           Sel50 = slider(min=0, max=Nage, initial=5, step=0.01),
           Sel95 = slider(min=0, max=Nage, initial=10, step=0.01))

# WHAT HAPPENS IF Sel50 > Sel95?

# Create function to predict numbers at age ==============================================
# Ok, next create a function to simulate numbers at age across time
# This function will return a matrix of model-predicted numbers at age N[year,age]

# Lets explore how we do these calculations, given some trial parameter values
# Nyear, Nage, ages,
# Mval, # Natural mortality (single value)
Fmort <- rep(0.1,Nyear)   # Fishing Mortality (vector of annual F rates)
Sel50 <- 4 
Sel95 <-  10# Selectivity parameters
N1 <- rep(100, Nage)  # Vector of estimated initial numbers at age for the 1st year
R <- rep(100,Nyear)


# Create Numbers at age matrix for storing predictions N[year,age]
N <- matrix(nrow=Nyear, ncol=Nage+1,
            dimnames=list(c(1:Nyear), paste0("Age-",0:Nage)))
N

# Calculate Selectivity at age
S <- select_fxn(Age=ages, Sel50=Sel50, Sel95=Sel95)
S
cbind(ages,S)

# Calculate Instantaneous Fishing mortality by year and age
F_ta <- matrix(nrow=Nyear, ncol=Nage+1,
               dimnames=list(c(1:Nyear), paste0("Age-",0:Nage)))
for(y in 1:Nyear) {
  for(a in 1:(Nage+1)) {
    F_ta[y,a] <- S[a] * Fmort[y]
  } #next age
} # next year

F_ta

# Calculate total mortality 
Z <- F_ta + Mval
Z

# Populate initial abundance in the first year for ages 1-Nage
for(a in 2:(Nage+1)) {
  N[1,a] <- N1[a-1]
}

N

# Populate the recruits (age=0) for all years
for(y in 1:Nyear) {
  N[y,1] <- R[y]
}

N

# Project the Numbers at age forward in time
y <- 1
for(y in 1:(Nyear-1)) {
  a <- 1
  for(a in 1:Nage) {
    if(a < Nage) {
      
      N[y+1, a+1] <- N[y,a]*exp(-Z[y,a])
      
    } else { # Calculations for the plus age group
      
      N[y+1, a+1] <- N[y,a]*exp(-Z[y,a]) +
                      N[y,a+1]*exp(-Z[y,a+1])
      
    }
  } #next a
} # next year

# Now lets look at our predicted numbers at age matrix
N



# Now that we understand the calculations, we can package it all in a nice clean function...
Numbers <- function(Nyear, Nage, ages,
                    Mval, # Natural mortality (single value)
                    Fmort,  # Fishing Mortality (vector of annual F rates)
                    Sel50, Sel95, # Selectivity parameters
                    N1, # Vector of estimated initial numbers at age for the 1st year
                    R) { # Vector of estimated recruitments by year
  
  
  # Create Numbers at age matrix for storing predictions N[year,age]
  N <- matrix(nrow=Nyear, ncol=Nage+1,
              dimnames=list(c(1:Nyear), paste0("Age-",0:Nage)))
  
  # Calculate Selectivity at age
  S <- select_fxn(Age=ages, Sel50=Sel50, Sel95=Sel95)
  
  # Calculate Instantaneous Fishing mortality by year and age
  F_ta <- matrix(nrow=Nyear, ncol=Nage+1,
                 dimnames=list(c(1:Nyear), paste0("Age-",0:Nage)))
  for(y in 1:Nyear) {
    for(a in 1:(Nage+1)) {
      F_ta[y,a] <- S[a] * Fmort[y]
    } #next age
  } # next year
  
  # Calculate total mortality 
  Z <- F_ta + Mval
  
  # Populate initial abundance in the first year for ages 1-Nage
  for(a in 2:(Nage+1)) {
    N[1,a] <- N1[a-1]
  }
  
  # Populate the recruits (age=0) for all years
  for(y in 1:Nyear) {
    N[y,1] <- R[y]
  }
  
  # Project the Numbers at age forward in time
  y <- 1
  for(y in 1:(Nyear-1)) {
    a <- 1
    for(a in 1:Nage) {
      if(a < Nage) {
        
        N[y+1, a+1] <- N[y,a]*exp(-Z[y,a])
          
      } else { # Calculations for the plus age group
        
        N[y+1, a+1] <- N[y,a]*exp(-Z[y,a]) +
                       N[y,a+1]*exp(-Z[y,a+1])
        
      }
    } #next a
  } # next year
  # RETURN SECTION
  
  # Here we will return a list with different elements that we have previously calculated
  # 1) N: numbers-at-age
  # 2) S: selectivity-at-age
  # 3) F_ta: Fishing mortality by year and age
  # 4) Z: Matrix to total instantaneous mortality
  
  # We can bind these together as a list object to be returned by the function
  
  out <- NULL
  out$N <- N
  out$S <- S
  out$F_ta <- F_ta
  out$Z <- Z
  return(out)
}


# Test function creating predicted numbers at age ==============================
out <- Numbers(Nyear=Nyear, Nage=Nage, ages=ages,
              Mval=Mval,
              Fmort=rep(0.1,Nyear),
              Sel50=4, Sel95=10,
              N1=rep(10, Nage+1),
              R=rep(100,Nyear) )

# Our function returns a list or predicted quantities.

# Lets inspect the list object that is returned from our new function
typeof(out)  # What is its object type?
str(out)  # What is its structure?
out  # What does it actually return?


# This is how we can extract the individual elements of the list
#   we bound together to return from the function
N <- out$N
N

S <- out$S
S

F_ta <- out$F_ta
F_ta

Z <- out$Z
Z

# Lets create a function to plot our population over time,
#   Given parameter values
plot_Numbers <- function(Nyear=Nyear, Nage=Nage, ages=ages,
                         Mval=Mval,
                         Fmort=rep(0.1,Nyear),
                         Sel50=4, Sel95=10,
                         N1=rep(100, Nage+1),
                         R=rep(100,Nyear)) {
  # Predict Numbers at Age
  N <- Numbers(Nyear=Nyear, Nage=Nage, ages=ages,
               Mval=Mval,
               Fmort=Fmort,
               Sel50=Sel50, Sel95=Sel95,
               N1=N1,
               R=R)$N
  
  # Convert to a data frame for easier plotting
  N.df <- melt(N)
  names(N.df) <- c("Year","Age","N")
  
  # Plot
  g.N <- ggplot(data=N.df, aes(x=Year, y=N, fill=Age)) +
           theme_linedraw() +
           scale_fill_viridis_d() +
           geom_area()
  g.selex <- plot_select(Age=ages, Sel50, Sel95)
  
  # Combined plot
  plot_grid(g.N, g.selex, ncol=2, rel_widths = c(2,1))
  
}

# Now lets explore population dynamics under different selectivity (Sel50, Sel95) parameters with manipulate
manipulate(plot_Numbers(Nyear=Nyear, Nage=Nage, ages=ages,
                                    Mval=Mval,
                                    Fmort=rep(0.1,Nyear),
                                    Sel50, Sel95,
                                    N1=rep(100, Nage+1),
                                    R=rep(100,Nyear)),
           Sel50 = slider(min=0, max=Nage, initial=5, step=0.01),
           Sel95 = slider(min=0, max=Nage, initial=10, step=0.01)
           
           )

# NOTICE THAT WHEN WE START WITH EQUAL NUMBERS AT AGE IN YEAR 1
#   WE ACCUMULATE LARGE ABUNDANCE IN THE "PLUS GROUP" (AGE 12)
#   IN THE FIRST 5-10 YEARS

# Functions to Generate Expected Values ==================================

# Now that we have a function or projecting our numbers-at-age, lets create a  function
#   to extract the model-predicted quantities we will compared to our observed data.

#   The arguments for this function would be:
#     1) The numbers-at-age matrix 
#     2) The weight-at-age vector
#     3) The instantaneous fishing mortality matrix by year and age
#     4) The total instantaneous mortality matrix by year and age
#     5) The selectivity at age

# This function will calculate and return model-predicted quantities:
#   1) Catch_pred: The model-predicted annual catch in units of biomass
#   2) Catch_ta: Model-predicted catch (numbers) by year and age
#   3) Bio: Survey biomass by year
#   4) Propn_pred: Model-predicted CATCH age composition proportions by year and age

extract_quants <- function(N, waa, F_ta, Z, S) {
  
  # Create vector of model-predicted catch in units of weight
  #   It will have length equal to the number of rows in the N matrix (which is years)
  Catch_pred <- rep(0, nrow(N))
  
  # Create a matrix to hold catch (numbers) by year and age
  #   it will have the same dimensions as our N matrix
  Catch_ta <- matrix(nrow=nrow(N), ncol=ncol(N))
  
  # Survey biomass
  Bio <- rep(0, nrow(N))
  
  # Age composition proportions from Catch
  Propn_pred <- matrix(nrow=nrow(N), ncol=ncol(N))
  
  # Loop through years and multiply numbers-at-age by weight-at-age,
  #   then sum to get total catch
  y <- 1
  for(y in 1:nrow(N)) {
    a <- 1
    for(a in 1:ncol(N)) {
      
      # Catch year-age (numbers)
      Catch_ta[y,a] <- (F_ta[y,a]/Z[y,a])*N[y,a]*(1-exp(-Z[y,a]))
      
      # Annual catch total (biomass)
      Catch_pred[y] <- Catch_pred[y] + Catch_ta[y,a]*waa[a]
      
      # Survey biomass
      Bio[y] <- Bio[y] + N[y,a]*waa[a]*S[a] #Same selectivity assumed for fishery and survey
      
    } # next a
    
    # Convert catch age to annual proportions
    Propn_pred[y,] <- Catch_ta[y,]/sum(Catch_ta[y,])
  } # next y
  
  # Return Section 
  #   Here we will again use a list to package all of the objects we want to return
  returns <- NULL
  returns$Catch_ta <- Catch_ta
  returns$Catch_pred <- Catch_pred
  returns$Bio <- Bio
  returns$Propn_pred <- Propn_pred
  return(returns)
}

# Let's test it!
returns <- extract_quants(N=N, waa=waa, F_ta=F_ta, Z=Z, S=S)

# Inspect the elements
typeof(returns)
names(returns)
str(returns)

# Now lets practice extracting the elements
Catch_ta <- returns$Catch_ta
Catch_ta

Catch_pred <- returns$Catch_pred
Catch_pred

Bio <- returns$Bio
Bio

Propn_pred <- returns$Propn_pred
Propn_pred

# Function to Calculate Negative Log likelihood for fitting age-structured stock assessment ====================

# Phew, that was a bit of work, but now we have functions to :
# a) Predict numbers at age (N), fishing mortality by year and age (F_ta), selectivity (S),
#      and total mortality (Z).
# b) Extract key quantities of interest, that we will compare with our data

# Lets separate the data we previously loaded into separate objects
Catch_obs <- CatchCPUE$Catch
Catch_obs

CPUE_obs <- CatchCPUE$CPUE
CPUE_obs

Propn_obs <- Propn[,-1] #Removing the first column that contains year references
Propn_obs

# This function will need to have all of our data objects (Nyear, Nage, ages,
#   Mval, Omega, waa, Catch_obs, CPUE_obs, Propn_obs), as well as the model parameters.
# As has become our practice we will estimate all of our parameters in log space.

par.names <- c("ln_q", "ln_Sel50", "ln_Sel95",
               paste0("ln_Fmort_", c(1:Nyear)),
               paste0("ln_N1_", c(1:Nage)),
               paste0("ln_R_", c(1:Nyear)))

par.names

# Given the number of parameters we are estimating
length(par.names) # Wow, 55 in total!

# And the fact that some of these are vectors (like our Fmorts) we need to use the optim() function to 
#  do our non-linear minimization

?optim

# optim() works a lot like mle2(), but expects all of our parameters to be specified in one long vector
#   to do this we will need to add an extra step where we combine all of our trial parameter values
#   in a common vector, then separate these our within our negative log likelihood function

# An easy way to find where various pieces are in our parameter vector is to use the 
#   grep() function
?grep

# grep() lets you match a pattern within the names of a vector, it is actually
#   a very powerful and useful function.
grep("q", par.names)
grep("Fmort", par.names)
grep("N1", par.names)
grep("R", par.names)

# So we will have one long vector or trial parameter values (called "pars") of length 55,
#   then in our first step, use the locations for each parameter 
#   we have from grep() to separate things out



NLL <- function(Nyear, Nage, ages,
                Mval, Omega,
                waa,
                Catch_obs, SigCatch,
                CPUE_obs, SigCPUE,
                Propn_obs,
                pars) { 
  
  
  # Extract parameters, based on their location
  ln_q <- pars[1]
  ln_Sel50 <- pars[2]
  ln_Sel95 <- pars[3]
  ln_Fmort <- pars[4:23]
  ln_N1 <- pars[24:35]
  ln_R <- pars[36:55]
  
  
  # Step 1: Exponentiate model parameters back into normal space
  q <- exp(ln_q)
  Sel50 <- exp(ln_Sel50)
  Sel95 <- exp(ln_Sel95)
  Fmort <- exp(ln_Fmort)
  N1 <- exp(ln_N1)
  R <- exp(ln_R)
  
  
  # Step 2: Predict numbers at age and other quantities
  out <- Numbers(Nyear=Nyear, Nage=Nage, ages=ages,
                 Mval=Mval,
                 Fmort=Fmort,
                 Sel50=Sel50, Sel95=Sel95,
                 N1=N1,
                 R=R )
  
  # Extract elements to be passed to extract_quants()
  N <- out$N          # Predicted numbers-at-age
  F_ta <- out$F_ta    # Fishing mortality by year and age
  Z <- out$Z          # Total mortality by year and age
  S <- out$S          # Selectivity at age
  
  # Step 3: Extract model-predicted quantities for comparison to our observed data
  returns <- extract_quants(N=N, waa=waa, F_ta=F_ta, Z=Z, S=S)
  
  # Extract quantities to be passed to SSQ functions
  Catch_pred <- returns$Catch_pred     # Predicted annual catch (kg)
  Bio <- returns$Bio                   # Predicted survey biomass
  Propn_pred <- returns$Propn_pred     # Predicted age composition proportions from catch
  
  # Next, lets calculate our predicted biomass index as: catchability (q) X Biom
  CPUE_pred <- q * Bio
  
  # Calculate Differences between observed (data) and model-predicted quantities
  #   and observed data
  
  
  
  # Catch (biomass): Approximate Normal Likelihood
  # SSQ_catch <- sum( ( Catch_obs-Catch_pred )^2 )
  # NLL_catch <- -1*sum( dnorm(x=Catch_obs,  mean=Catch_pred, sd=SigCatch, log=TRUE) )
  NLL_catch <- sum(((Catch_obs-Catch_pred)/Catch_pred)^2)/(2*SigCatch^2)
  
  # Survey CPUE: Approximate Lognormal Likelihood (with a small offset to avoid log(0) issues)
  # SSQ_CPUE <- sum( ( log(CPUE_obs+0.001)-log(CPUE_pred+0.001) )^2 )
  # NLL_CPUE <- -1*sum( dnorm(x=log(CPUE_obs+0.001),  mean=log(CPUE_pred+0.001), sd=SigCPUE, log=TRUE) )
  NLL_CPUE <- sum( (log(CPUE_obs+1e-6) - log(CPUE_pred+1e-6))^2 )/(2*SigCPUE^2)
  
  # Catch age composition: Multinomial likelihood
  LL_age <- 0  #Log-likelihood for age
  for(y in 1:Nyear) {
    for(a in 1:(Nage+1)) {
      if(Propn_obs[y,a] > 0) { # Ensure we don't try to take the log of 0!
        LL_age <- LL_age + Propn_obs[y,a] * log(Propn_pred[y,a] / Propn_obs[y,a] + 0.001)  
      }
    } # Next a
  } # next y
  
  # Calculate Negative log likelihood for agecomp, and multiply by agecomp weighting (Omega)
  NLL_age = -1*Omega*LL_age; 
  
  # Calcualte total objective function 
  objFxn <- NLL_catch + NLL_CPUE + NLL_age
  
  # Return the total objective function value
  return(objFxn)
}

# Set starting values for model parameters ==========================
# Now, lets set some reasonable starting values for our parameters
ln_q_start <- log(0.9)
ln_Sel50_start <- log(4.5)
ln_Sel95_start <- log(8)
ln_Fmort_start <- log(rep(0.5,Nyear))
ln_N1_start <- log(rep(5, Nage))
ln_R_start <- log(rep(8, Nyear))

# Combine into pars vector
pars_start <- c(ln_q_start, ln_Sel50_start, ln_Sel95_start,
                  ln_Fmort_start, ln_N1_start, ln_R_start)

pars_start


# OK, lets check to see if our negative log likelihood funciton works with our
#   parameter starting values...
NLL(Nyear=Nyear, Nage=Nage, ages=ages,
    Mval=Mval, Omega=Omega, waa=waa,
    Catch_obs=Catch_obs, SigCatch=SigCatch,
    CPUE_obs=CPUE_obs, SigCPUE=SigCPUE,
    Propn_obs=Propn_obs,
    pars=pars_start)


# NLL Function Minimization ===================================
# Much like mle2, we will specify the name of our function that 
#   returns the value to be minimized (NLL) to an argument called fn=

# We will specify the vector of parameters to estimate in optim()
#   with the par= argument

# The other arguments are our data inputs

fit_nll <- optim(par=pars_start, fn=NLL,
                 Nyear=Nyear, Nage=Nage, ages=ages,
                 Mval=Mval, Omega=Omega, waa=waa,
                 Catch_obs=Catch_obs, SigCatch=SigCatch,
                 CPUE_obs=CPUE_obs, SigCPUE=SigCPUE,
                 Propn_obs=Propn_obs, 
                 method="BFGS",
                 hessian=FALSE,
                 control=list(trace=TRUE, maxit=1e4))

# We can extract parameters from a fitted optim model using $par
fit_nll$par

# Lets re-run starting at the fitted values from the last round to see if we can get a better fit
#   This is called "Multiple Shooting" (Carl Walters)
fit_nll2 <- optim(par=fit_nll$par, fn=NLL,
                  Nyear=Nyear, Nage=Nage, ages=ages,
                  Mval=Mval, Omega=Omega, waa=waa,
                  Catch_obs=Catch_obs, SigCatch=SigCatch,
                  CPUE_obs=CPUE_obs, SigCPUE=SigCPUE,
                  Propn_obs=Propn_obs, 
                  method="BFGS",
                  hessian=FALSE,
                  control=list(trace=TRUE, maxit=1e4))

# EXPLORE DIFFERENT SEARCH METHODS TO SEE WHICH ARRIVE AT THE SAME
#   FINAL VALUE, AND WHICH ARE MORE/LESS EFFICIENT

# method="Nelder-Mead"
# method="SANN"

# Extract parameter values ====================================
# We can extract are maximum likelihood estimates for model parameters, using $par

# But, remember we specified these as a vector, so we will need to:
#   1) Sort these out and correctly assign them to the correct parameter names
#   2) Exponentiate the estimated model parameters, as they were estimated in log space



fit_q <- exp(fit_nll2$par[1])
fit_q

fit_Sel50 <- exp(fit_nll2$par[2])
fit_Sel50

fit_Sel95 <- exp(fit_nll2$par[3])
fit_Sel95

fit_Fmort <- exp(fit_nll2$par[4:23])
fit_Fmort

fit_N1 <- exp(fit_nll2$par[24:35])
fit_N1

fit_R <- exp(fit_nll2$par[36:55])
fit_R

# Plot Outputs ====================================================
# OK, now that we have fit our model to the data
#   lets get our predicted values and plot our model fit

# First lets plot the estimated selectivity
plot_select(Age=ages, fit_Sel50, fit_Sel95)


# Next, lets generate model predictions from our population dynamics model
#   so we can use the predicted values to compare our observed and predicted values
out <- Numbers(Nyear=Nyear, Nage=Nage, ages=ages,
               Mval=Mval,
               Fmort=fit_Fmort,
               Sel50=fit_Sel50, Sel95=fit_Sel95,
               N1=fit_N1,
               R=fit_R )


# Lets extract and explore our predicted quantities
N <- out$N # Numbers at age
N

S <- out$S # Selectivity at age
S

F_ta <- out$F_ta # Fishing mortality by year and age
F_ta

Z <- out$Z  # Total mortality by year and age
Z

# Plot predicted fishing mortality at age ======================
list_F_ta <- melt(F_ta)
names(list_F_ta) <- c("Year","Age","F")

ggplot(list_F_ta, aes(x=Age, y=Year, size=F, color=F)) +
  theme_linedraw() +
  scale_color_viridis_c() +
  geom_point()

# Plot Predicted Numbers at age ====================================
# Next, lets plot the timeseries of predicted numbers at age

# To do so we can use the plot_Numbers() function we created above

plot_Numbers(Nyear=Nyear, Nage=Nage, ages=ages,
                         Mval=Mval,
                         Fmort=fit_Fmort,
                         Sel50=fit_Sel50, Sel95=fit_Sel95,
                         N1=fit_N1,
                         R=fit_R)

# Plot Fits to Data ==============================================
# Next, lets plot our fit to the three data sources that informed our assessment model

# We can use the extract_quants() function to calculate the model-predicted quanties of interest
returns <- extract_quants(N=out$N, waa=waa, F_ta=out$F_ta, Z=out$Z, S=out$S)

# Now, lets extracted the predicted quanties
Catch_ta <- returns$Catch_ta # Catch by year and age
Catch_ta

Catch_pred <- returns$Catch_pred # Predicted annual yield
Catch_pred

Bio <- returns$Bio #Predicted survey biomass
Bio

# From this we can calculated our model predicted survey index
CPUE_pred <- Bio*fit_q 

Propn_pred <- returns$Propn_pred  # Predicted catch age composition proportions by year
Propn_pred

# First, lets plot the observed and predicted CPUE indices
dev.off()

# Observed CPUE
plot(CPUE_obs, type="h", xlab="Year", ylab="Survey CPUE",
       main="Fit to CPUE Data")
points(CPUE_obs, pch=21, bg="blue")
# Predicted CPUE
lines(CPUE_pred, lwd=3, col=rgb(1,0,0, alpha=0.5))
legend('topright', legend=c("Observed","Predicted"), fill=c("blue","red"))

# Second, lets plot the observed and predicted catch time series
# Observed catch
plot(Catch_obs, type="h", xlab="Year", ylab="Fishery Yield",
     main="Fit to Catch Data")
points(Catch_obs, pch=21, bg="blue")
# Predicted catch
lines(Catch_pred, lwd=3, col=rgb(1,0,0, alpha=0.5))
legend('topright', legend=c("Observed","Predicted"), fill=c("blue","red"))


# Plot Observed and PRedicted Age Composition ===============================

# First, to plot observed and predicted proportions in ggplot()
#  lets convert our values to lists

# Lets name the elements of our predicted proportions matrix
dimnames(Propn_pred) <- list(c(1:Nyear), paste0("Age_",c(0:12)))
Propn_pred

# And the same for the 
dimnames(Propn_obs) <- list(c(1:Nyear), paste0("Age_",c(0:12)))

# Next, we can convert our matrices of observed and predicted proportions 
#   to lists using the melt() function

list_Propn_pred <- melt(Propn_pred)
head(list_Propn_pred)

list_Propn_obs <- melt(as.matrix(Propn_obs))

# Now lets name the columns in our new data frames
names(list_Propn_pred) <- c("Year","Age","pred.prop")
names(list_Propn_obs) <- c("Year","Age","obs.prop")


# Now lets plot things
plot.age.pred <- ggplot(data=list_Propn_pred, aes(x=Year, y=pred.prop, fill=Age)) +
  theme_dark() +
  geom_col() +
  scale_fill_viridis_d() +
  ylab("Catch Age Composition Proportions") +
  ggtitle("Predicted")

plot.age.pred

plot.age.obs <- ggplot(data=list_Propn_obs, aes(x=Year, y=obs.prop, fill=Age)) +
  theme_dark() +
  geom_col() +
  scale_fill_viridis_d() +
  ylab("Catch Age Composition Proportions") +
  ggtitle("Observed")

plot.age.pred

# Now lets plot these figures side by side using the plot_grid() function in 
#   the cowplot library
library(cowplot)

plot_grid(plot.age.obs, plot.age.pred, align="v",ncol=1)

# There are of course other ways to plot the age composition fit
# Lets combine the observed and predicted values, by joining them together in a common
#   data fram for plotting

list_Propn_comb <- list_Propn_obs %>% left_join(list_Propn_pred)
head(list_Propn_comb)

ggplot(data=list_Propn_comb, aes(x=Age, y=obs.prop, fill=Age)) +
  theme_dark() +
  geom_col() +
  facet_wrap(~Year) +
  scale_fill_viridis_d(option="C") +
  # Predicted values as red points
  geom_point(aes(x=Age, y=pred.prop), color="green") +
  
  ylab("Age Composition Proportion") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90))


# Part B: Exploring Data Weighting ===================================
# Data weighting when fitting a model of this type is part art and part science.
# In an ideal world we could use model-based weighting to accurately weight our fit
#   to sometimes conflicting data.

# But, our estimates of uncertainty for different data aren't always accurate

# Lets explore alternative data weightings by INCREASING the weight we place on our age composition data
#   (Omega)

# Try refitting the model with Omega=5000 and plot the model fits to the data,

# WHAT HAPPENS? HOW DO YOUR FITS TO THE DIFFERENT DATA CHANGE?

# Omega=5000 ==================================================
fit_nll_Omega1 <- optim(par=pars_start, fn=NLL,
                 Nyear=Nyear, Nage=Nage, ages=ages,
                 Mval=Mval, Omega=5000, waa=waa,
                 Catch_obs=Catch_obs, SigCatch=SigCatch,
                 CPUE_obs=CPUE_obs, SigCPUE=SigCPUE,
                 Propn_obs=Propn_obs, 
                 method="BFGS",
                 hessian=FALSE,
                 control=list(trace=TRUE, maxit=1e4))


fit_q_Omega1 <- exp(fit_nll_Omega1$par[1])
fit_q_Omega1

fit_Sel50_Omega1 <- exp(fit_nll_Omega1$par[2])
fit_Sel50_Omega1

fit_Sel95_Omega1 <- exp(fit_nll_Omega1$par[3])
fit_Sel95_Omega1

fit_Fmort_Omega1 <- exp(fit_nll_Omega1$par[4:23])
fit_Fmort_Omega1

fit_N1_Omega1 <- exp(fit_nll_Omega1$par[24:35])
fit_N1_Omega1

fit_R_Omega1 <- exp(fit_nll_Omega1$par[36:55])
fit_R_Omega1

# Plot Outputs ====================================================
# OK, now that we have fit our model to the data
#   lets get our predicted values and plot our model fit

# First lets plot the estimated selectivity
plot_select(Age=ages, fit_Sel50_Omega1, fit_Sel95_Omega1)


# Next, lets generate model predictions from our population dynamics model
#   so we can use the predicted values to compare our observed and predicted values
out_Omega1 <- Numbers(Nyear=Nyear, Nage=Nage, ages=ages,
               Mval=Mval,
               Fmort=fit_Fmort_Omega1,
               Sel50=fit_Sel50_Omega1, Sel95=fit_Sel95_Omega1,
               N1=fit_N1_Omega1,
               R=fit_R_Omega1 )



# We can use the extract_quants() function to calculate the model-predicted quanties of interest
returns_Omega1 <- extract_quants(N=out_Omega1$N, waa=waa, F_ta=out_Omega1$F_ta, Z=out_Omega1$Z, S=out_Omega1$S)

# Now, lets extracted the predicted quanties
Catch_ta_Omega1 <- returns_Omega1$Catch_ta # Catch by year and age
Catch_ta_Omega1

Catch_pred_Omega1 <- returns_Omega1$Catch_pred # Predicted annual yield
Catch_pred_Omega1

Bio_Omega1 <- returns_Omega1$Bio #Predicted survey biomass
Bio_Omega1

# From this we can calculated our model predicted survey index
CPUE_pred_Omega1 <- Bio_Omega1*fit_q_Omega1 

Propn_pred_Omega1 <- returns_Omega1$Propn_pred  # Predicted catch age composition proportions by year
Propn_pred_Omega1

# Observed CPUE
plot(CPUE_obs, type="h", xlab="Year", ylab="Survey CPUE",
     main="Fit to CPUE Data")
points(CPUE_obs, pch=21, bg="blue")
# Predicted CPUE
lines(CPUE_pred_Omega1, lwd=3, col=rgb(1,0,0, alpha=0.5))
legend('topright', legend=c("Observed","Predicted"), fill=c("blue","red"))

# Second, lets plot the observed and predicted catch time series
# Observed catch
plot(Catch_obs, type="h", xlab="Year", ylab="Fishery Yield",
     main="Fit to Catch Data")
points(Catch_obs, pch=21, bg="blue")
# Predicted catch
lines(Catch_pred_Omega1, lwd=3, col=rgb(1,0,0, alpha=0.5))
legend('topright', legend=c("Observed","Predicted"), fill=c("blue","red"))


# Plot Observed and PRedicted Age Composition ===============================

dimnames(Propn_pred_Omega1) <- list(c(1:Nyear), paste0("Age_",c(0:12)))
Propn_pred_Omega1

list_Propn_pred_Omega1 <- melt(Propn_pred_Omega1)
head(list_Propn_pred_Omega1)

names(list_Propn_pred_Omega1) <- c("Year","Age","pred.prop")


# Now lets plot things
plot.age.pred_Omega1 <- ggplot(data=list_Propn_pred_Omega1, aes(x=Year, y=pred.prop, fill=Age)) +
  theme_dark() +
  geom_col() +
  scale_fill_viridis_d() +
  ylab("Catch Age Composition Proportions") +
  ggtitle("Predicted")

plot.age.pred_Omega1

plot.age.obs <- ggplot(data=list_Propn_obs, aes(x=Year, y=obs.prop, fill=Age)) +
  theme_dark() +
  geom_col() +
  scale_fill_viridis_d() +
  ylab("Catch Age Composition Proportions") +
  ggtitle("Observed")

plot.age.pred

# Now lets plot these figures side by side using the plot_grid() function in 
#   the cowplot library
library(cowplot)

plot_grid(plot.age.obs, plot.age.pred_Omega1, align="v",ncol=1)

# There are of course other ways to plot the age composition fit
# Lets combine the observed and predicted values, by joining them together in a common
#   data fram for plotting

list_Propn_comb_Omega1 <- list_Propn_obs %>% left_join(list_Propn_pred_Omega1)
head(list_Propn_comb)

ggplot(data=list_Propn_comb_Omega1, aes(x=Age, y=obs.prop, fill=Age)) +
  theme_dark() +
  geom_col() +
  facet_wrap(~Year) +
  scale_fill_viridis_d(option="C") +
  # Predicted values as red points
  geom_point(aes(x=Age, y=pred.prop), color="green") +
  
  ylab("Age Composition Proportion") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90))


# Part C: Simulating and Refitting =====================================

# In order to construct our estimation model we had to build **most** of the pieces necessary to simulate new data. 

# In the final portion of this lab, we will:
#   1) Generate new data with known values for model parameters
#   2) Add random error to those data
#   3) Refit our model to see if we can estimate (Recover) the "true" values 
#        for those parameters


# First, lets define new parameter values

# For simplicity we will keep our number of years and ages the same as well as the assumed natural mortality
Nyear
Nage
ages
Mval

# But lets set the true value for our selectivity parameters
true.Sel50 <- 3
true.Sel95 <- 10

# The true value for catchability 
true.q <- 0.5

# The true value for our fishing mortality parameters
true.Fmort <- c(rep(0.1,5), rep(0.5,5), rep(0.1,5), rep(0.5,5))
true.Fmort
plot(true.Fmort, type="b")

# The true recruitment - We will just make some random lognormal deviates
set.seed(101)
true.R <- exp(rnorm(n=Nyear, mean=1, sd=1))
true.R
plot(true.R, type="b")

# The true initial numbers at age
#   Becuase this is a rather boring set of parameters lets just set them
#     to the estimate values above
true.N1 <- fit_N1
true.N1
plot(true.N1, xlab="Age Class")

# Second, lets simulate new numbers at age

sim.out <- Numbers(Nyear=Nyear, Nage=Nage, ages=ages,
                      Mval=Mval,
                      Fmort=true.Fmort,
                      Sel50=true.Sel50, Sel95=true.Sel95,
                      N1=true.N1,
                      R=true.R )


# Extract elements of the simulated data
true.N <- sim.out$N          # Predicted numbers-at-age
true.F_ta <- sim.out$F_ta    # Fishing mortality by year and age
true.Z <- sim.out$Z          # Total mortality by year and age
true.S <- sim.out$S          # Selectivity at age

# Third, lets calculate our predicted quantities that will form 
#   the input data for estmation
true.returns <- extract_quants(N=true.N, waa=waa, F_ta=true.F_ta, Z=true.Z, S=true.S)

true.Catch <- true.returns$Catch_pred     # Predicted annual catch (kg)
true.Bio <- true.returns$Bio                   # Predicted survey biomass
true.Propn <- true.returns$Propn_pred     # Predicted age composition proportions from catch

# Calculate our true index
true.CPUE <- true.Bio*true.q

# Next, we need to add some error to our true values
#   We will consider these our simulated data objects

# Add normal error
dat.Catch <- true.Catch + rnorm(n=length(true.Catch), mean=0, sd=SigCatch)

# Add lognormal error
dat.CPUE <- exp( log(true.CPUE) + rnorm(n=length(true.CPUE), mean=0, sd=SigCPUE))

# Add multinomial error
dat.Propn <- array(dim=dim(true.Propn))

?rmultinom

i <- 1
for(i in 1:nrow(true.Propn)) {
  # Random multinomial sample with sample size of 1000
  #   based on true proportions
  samp <- rmultinom(n=1, size=100,prob=true.Propn[i,])[,1]
  dat.Propn[i,] <- samp/sum(samp) #Convert back to proportions
} #next i
dat.Propn


par(mfrow=c(2,1))
# True Proportions
barplot(t(true.Propn))

# "Observed" Proportions
barplot(t(dat.Propn))


# Excellent!

# We have simulated new data based on true values for model parameters
#   and added error to these data.

# Now, lets refit our model and see how well we can estimate our true parameter values

# Test fit to simulated data ==================================
test.fit <- optim(par=pars_start, fn=NLL,
                 Nyear=Nyear, Nage=Nage, ages=ages,
                 Mval=Mval, Omega=Omega, waa=waa,
                 Catch_obs=dat.Catch, # UPDATED FOR SIMULATED DATA
                 SigCatch=SigCatch,
                 CPUE_obs=dat.CPUE, # UPDATED FOR SIMULATED DATA
                 SigCPUE=SigCPUE,
                 Propn_obs=dat.Propn, # UPDATED FOR SIMULATED DATA
                 method="BFGS",
                 hessian=FALSE,
                 control=list(trace=TRUE, maxit=1e5))

# EXTRACT PARAMETER VALUES AND COMPARE WITH TRUTH
test_q <- exp(test.fit$par[1])
test_q

true.q

test_Sel50 <- exp(test.fit$par[2])
test_Sel50

true.Sel50

test_Sel95 <- exp(test.fit$par[3])
test_Sel95

true.Sel95

test_Fmort <- exp(test.fit$par[4:23])
test_Fmort

true.Fmort

test_N1 <- exp(test.fit$par[24:35])
test_N1

true.N1

test_R <- exp(test.fit$par[36:55])
test_R

true.R


# NOW PLEASE EXPLORE WHAT HAPPENS WHEN:

#   YOU REDUCE THE MULTINOMIAL SAMPLE SIZE rmultinom(n=1, size=1000,prob=true.Propn[i,])[,1]

#   YOU CHANGE THE SIMULATED Fmort TRAJECTORY

