#==================================================================================================
#Project Name: BRISTOL BAY INSEASON FORECAST - Integrated Bayesian Inseason Model - v17
#Creator: Curry James Cunningham, College of Fisheries and Ocean Sciences, UAF
#Date: 6.19.21
#
#Purpose: To generate predictions from an integrated Bayesian inseason assessment model
#
#  1) Read in data
#  2) Interpolate Port Moller CPUE with GAM (current and past years)
#  3) Partition Port Moller CPUE based on GSI and Age Data
#  4) Stan model that estimates total run size
#
#
#==================================================================================================
#NOTES:
# This version adds
#  1) Port Moller daily CPUE index slope predictor
# 
# Next steps:
#  1) Add intercept to C+E relationships - DONE
#  2) Add intercept to cumulative PM relationships - DONE
#  3) Run timing enhanced C+E predictor (fit normal to PM CPUE)
#  4) Inseason estimation of RPI and TT to update C+E prediction
#
#==================================================================================================
require(BEST)
require(rstan)
# require(rstanarm)
require(tidyverse)
require(mgcv)
require(ggthemes)
require(viridis)
require(shinystan)
require(lubridate)
require(beepr)
require(tidyverse)
require(reshape2)
require(dplyr)
require(ggthemes)
require(tidybayes)
require(loo)
require(here)
require(forcats)

# CURRENTLY NEEDED WITH NEW R FOR PARALLELIZATION
# if (Sys.getenv("RSTUDIO") == "1" && !nzchar(Sys.getenv("RSTUDIO_TERM")) && 
#     Sys.info()["sysname"] == "Darwin" && getRversion() == "4.0.0") {
#   parallel:::setDefaultClusterOptions(setup_strategy = "sequential")
# }

options(mc.cores = parallel::detectCores())

rstan_options(javascript=FALSE)

# Define Workflow Paths ============================================
wd <- "/Users/curryc2/Documents/Bristol Bay Forecast/2012 Inseason"
setwd(wd)
# wd <- here()

dir.output <- file.path(wd,"Bayes-Integrated","output")
dir.figs <- file.path(wd,"Bayes-Integrated","figs")
dir.stan <- file.path(wd,"Bayes-Integrated","Stan")

dir.data <- file.path(wd,"Inseason Data")
dir.R <- file.path(wd,"Daily Forecast Plots","R")

# CONTROL SECTION ==================================================
myYear <- 2023 #2014, 2018
myDay <- 190 #Works after 171?

# Model Version
version <- 23

# Do we fit the model, or just load saved .rds outputs
fit <- TRUE 

# Use Brood table data as runsize target (TRUE), or use total CE as runsize target (FALSE)
run.target.bt <- FALSE 

# MCMC Parameters
n.chains <- 4
n.iter <- 1e4#2e4#5e4
n.thin <- 2

#Maximum Station
max.stat <- 18 #14, 18, 24

# Start Years for Predictors
startYearPM <- 1990
startYearPF <- 1993
startYearCE <- 1990#1980

# Adjust PM CPUE 2020+ Deeper Net
adj.pm.cpue <- FALSE
# adj.pm.cpue.mult <- 6/11 # Change from 6m depth to 11m depth
# adj.pm.cpue.mult <- 0.76 # 2020 Year-end Report

# Source Functions =================================================
# Daily Forecast Plots Folder
# source(file.path(dir.R,"interp.cpue.R"))
source(file.path(dir.R,"interp.cpue.gam.R"))
source(file.path(dir.R,"fill.ce.hist.R")) # Creates matrix 
source(file.path(dir.R,"calc-ce-props.R"))

source(file.path(dir.R,"calc-PF-variance.R"))
source(file.path(dir.R,"split.cpue.age.R"))

source(file.path(dir.R, "run-slope-analysis.R"))

# Load Data ========================================================
#Preseason Forecast
pf <- data.frame(read.table(file.path(dir.data,"preseasonForecast.dat"),skip=6))
names(pf) <- c('agency','forecaster','retYr','dist','stream','fwA','oA','ret')

# Load C and E Data
ce.data <- data.frame(read.table(file.path(dir.data,"ce2013.dat"), skip=1))
names(ce.data) <- c('year','dist','river','jdate','spec','ce','number')

# Brood Table Data
brood.dat <- data.frame(read.table(file.path(dir.data, "broodTable.dat"), skip=1))
names(brood.dat) <- c('broodYr','dist','stream','fwAge','oAge','return')
retYr <- brood.dat$broodYr + brood.dat$fwAge + brood.dat$oAge + 1
brood.dat <- data.frame(brood.dat,retYr)

# PM cpue
cpue <- data.frame(read.table(file.path(dir.data,"CPUE_new.dat"), skip=2))
names(cpue) <- c('year','day','station','cpue')
cpue$day <- cpue$day+160
# Adjust for deeper net
if(adj.pm.cpue==TRUE) {
  cpue$cpue[cpue$year>=2020] <- cpue$cpue[cpue$year>=2020] * adj.pm.cpue.mult
}

# PM genetic data
gen.dat <- data.frame(read.table(file.path(dir.data,"gen.dat"), skip=1))
names(gen.dat) <- c('year','dist','stream','start','end','prop','numb')

# PM agecomp
age.dat <- data.frame(read.table(file.path(dir.data,"oTFage.dat"), skip=3))
names(age.dat) <- c('year','dist','start','end','fwa','oa','number')
age.dat <- age.dat %>% group_by(year, start) %>% mutate(prop=number/sum(number))

# Metadata =========================================================
#Districts
dists <- c(321,322,324,325)
n.dists <- length(dists)
dist.names <- c('Ugashik','Egegik','Naknek-Kvichak','Nushagak') 

# Main Age Classes
ages <- c('1.2','1.3','2.2','2.3')
n.ages <- 4
fw.ages <- c(1,1,2,2)
o.ages <- c(2,3,2,3)

#Create Matrix of Historical CPUE
# years <- admb.out$startYearPM:admb.out$endYearPM
# n.years <- length(years)

pm.days <- min(cpue$day):max(cpue$day)
n.pm.days <- length(pm.days)

#Days with cpue for current year
curr.pm.days <- min(pm.days):min(myDay, max(pm.days))
n.curr.pm.days <- length(curr.pm.days)

#Catch and escapement days
ce.days <- min(ce.data$jdate):max(ce.data$jdate)
n.ce.days <- length(ce.days)

curr.ce.days <- min(ce.days):min(myDay, max(ce.data$jdate))
n.curr.ce.days <- length(curr.ce.days)

# Prediction Data years
yearPF <- startYearPF:(myYear-1)
nYearPF <- length(yearPF)

yearCE <- startYearCE:(myYear-1)
nYearCE <- length(yearCE)

yearPM <- startYearPM:(myYear-1)
nYearPM <- length(yearPM)

# Calculate Observed Run Sizes in CE and PM years ==================
if(run.target.bt==TRUE) {
  Robs.ce <- brood.dat %>% filter(retYr>=startYearCE, retYr<myYear) %>% group_by(retYr) %>%
    summarize(total=sum(return)) %>% arrange(retYr)
}else {
  Robs.ce <- ce.data %>% filter(year>=startYearCE, year<myYear) %>% group_by(year) %>%
    summarize(total=sum(number)) %>% arrange(year)
}
Robs.ce <- as.numeric(Robs.ce$total)

# if(run.target.bt==TRUE) {
  Robs.pm <- brood.dat %>% filter(retYr>=startYearPM, retYr<myYear) %>% group_by(retYr) %>%
    summarize(total=sum(return)) %>% arrange(retYr)
# }else {
  # Robs.pm <- ce.data %>% filter(year>=startYearPM, year<myYear) %>% group_by(year) %>%
    # summarize(total=sum(number)) %>% arrange(year)
# }
Robs.pm <- as.numeric(Robs.pm$total)

# By Age
Robs.pm.age <- array(dim=c(nYearPM,n.ages))
for(y in  1:nYearPM) {
  year <- yearPM[y]
  for(a in 1:n.ages) {
    # if(run.target.bt==TRUE) {
      Robs.pm.age[y,a] <- sum(brood.dat$return[brood.dat$retYr==year & 
                                               brood.dat$fwAge==fw.ages[a] &
                                               brood.dat$oAge==o.ages[a]])
    # }else {
    #   Robs.pm.age[y,a] <- sum(ce.data$return[brood.dat$retYr==year &
    #                                              brood.dat$fwAge==fw.ages[a] &
    #                                              brood.dat$oAge==o.ages[a]])
    # }
  }
}

# Partition CPUE ===================================================
out.cpue <- interp.cpue.gam(myDay=myDay, myYear=myYear,
                            cpue=cpue, gen.dat=gen.dat,
                            max.stat=max.stat,
                            minYear=startYearPM,
                            logLink=FALSE)

# plot(x=out.cpue$curr.pm.days, y=out.cpue$curr.cpue.daily, type='l')
# plot(x=out.cpue$pm.days, y=out.cpue$curr.cpue.extra.daily, type='l')

curr.cpue.daily.obs <- out.cpue$curr.cpue.daily.obs
hist.cpue.daily.obs <- out.cpue$hist.cpue.daily.obs

# Partition CPUE by age ============================================
# mismatch in dimension declared and found in context; processing stage=data initialization; variable name=curr_cpue_daily_obs; position=0; dims declared=(36); dims found=(33)

out.age.cpue <- split.cpue.age(out.cpue=out.cpue, #
                               age.dat=age.dat,
                               n.ages=n.ages,
                               ages=ages,
                               fw.ages=fw.ages,
                               o.ages=o.ages)

curr.age.cpue.obs <- out.age.cpue$curr.age.cpue.obs
hist.age.cpue.obs <- out.age.cpue$hist.age.cpue.obs


# Preseason Forecast Calculations ==================================
out.pf <- calc_PF_variance(myYear=myYear, startYearPF=startYearPF, 
                           brood.dat=brood.dat, pf=pf, baywide=TRUE)

mu.pf <- out.pf$mu.pf
sigma.pf <- out.pf$sigma.pf
curr.pf <- out.pf$curr.pf

# Catch and Escapement Calculations ================================
ce.props <- calc_ce_props(years=c(startYearCE:(myYear-1)), myDay=myDay, myYear=myYear, 
                          ce.data=ce.data, brood.dat=brood.dat, run.target.bt=run.target.bt)


# # NLS Fit for sigmoid
sig.data <- data.frame(ce.props$ce.days, ce.props$bay.days.props)
names(sig.data) <- list("days", "props")
# Subset
sig.data <- sig.data[sig.data$days>=min(pm.days) & sig.data$days<=212,]
plot(props~days, data=sig.data)
# 
fit.sig <- nls(props ~ 1/(1+exp(-delta*(days-a50))), data=sig.data, start=list(delta=1, a50=195), 
               control=list(maxiter=500))
hist(resid(fit.sig))
plot(fit.sig)

plot(props~days, data=sig.data)
lines(x=sig.data$days, y=predict(fit.sig), col="red")

#####

ce.years <- c(startYearCE:(myYear-1))
n.ce.years <- length(ce.years)

avg.prop.bay <- ce.props$avg.prop.bay
sd.prop.bay <- sd(ce.props$prop.ce.bay) #sqrt(sum((ce.props$prop.ce.bay-avg.prop.bay)^2)/(n.ce.years-1))

# Calculate Parameters for Beta Prior
# prior.ce.alpha <- avg.prop.bay*(((avg.prop.bay*(1-avg.prop.bay))/(sd.prop.bay^2))-1) #Incorrect
# prior.ce.beta <- 1-avg.prop.bay*(((avg.prop.bay*(1-avg.prop.bay))/(sd.prop.bay^2))-1)

prior.ce.alpha <- ((1 - avg.prop.bay) / sd.prop.bay^2 - 1 / avg.prop.bay) * avg.prop.bay ^ 2
prior.ce.beta <- prior.ce.alpha * (1 / avg.prop.bay - 1)

# hist(rbeta(1e3, prior.ce.alpha, prior.ce.beta))

# Current Year
curr.ce <- ce.props$curr.ce.bay

# Historical Years
hist.ce <- ce.props$hist.ce.bay


# Run Slope Analysis ===========================================================
out.slope.analysis <- run_slope_analysis(out.cpue=out.cpue, myDay=myDay, myYear=myYear, Robs.pm=Robs.pm)

# We will use best.gam outputs
sigma_PMslope <- out.slope.analysis$sigma.best.gam
pred_PMslope <- out.slope.analysis$pred.best.gam

# Extract ADMB Run Timing Predictions (for Priors) =============================
# Read ADMB Data
require(PBSmodelling)
admb.out <- readList(file.path(wd,"curryFcst","curryFcst.out"))
locs.admb <- which(admb.out$startYearPM:admb.out$endYearPM %in% c(startYearPM:(myYear-1)))

# Extract Mean and SD 
prior.muDay.mean <- mean(admb.out$PMmuDay[locs.admb])
prior.muDay.sd <- sd(admb.out$PMmuDay[locs.admb])

prior.sigmaDay.mean <- mean(admb.out$PMsigmaDay[locs.admb])
prior.sigmaDay.sd <- sd(admb.out$PMsigmaDay[locs.admb])

# CURRY: MIGHT CONSIDER USING nls() for simplified estimation without hangups of bivariate normal in ADMB

# Get Run Timing Data ==========================================================
timing.dat <- read.csv(file=file.path(dir.data, "Tables", "Median Table.csv"))

obs_ce_time <- timing.dat$bay[timing.dat$year %in% yearPM]
obs_pm_time <- admb.out$PMmuDay[locs.admb] # May need to be revisited

lm.timing <- lm(obs_ce_time ~ obs_pm_time)
summary(lm.timing)

if(startYearPM!=startYearCE) { stop("Current Way of Approximating C+E Prediction depends on them being the same.") }

# Plot relationship between prop through today and timing =====
prop <- ce.props$prop.bay$prop
dev <- obs_ce_time - mean(obs_ce_time)
dev.pm <- obs_pm_time - mean(obs_pm_time)
logis.dat <- data.frame(dev,prop,dev.pm)

plot(prop~dev, data=logis.dat)

# Estimate Delta =========================

# Fit both the delta and props
# nls.delta <- nls(prop ~ 1/(1+exp(-delta_logis*((log((1-prop_logis)/prop_logis)/(-delta_logis))-dev))), data=logis.dat, start=list(delta_logis=0.4, prop_logis=ce.props$avg.prop.bay))
# 
# plot(prop~dev, data=logis.dat)
# points(x=dev, y=predict(nls.delta), col="red")

# delta_logis  <- as.numeric(coef(nls.delta.2))[1]
# prop_logis  <- as.numeric(coef(nls.delta.2))[2]

# Fit both the delta and props (to PM deviations)
nls.delta.pm <- nls(prop ~ 1/(1+exp(-delta_logis*((log((1-prop_logis)/prop_logis)/(-delta_logis))-dev.pm))), 
                     data=logis.dat, start=list(delta_logis=0.4, prop_logis=ce.props$avg.prop.bay),
                     control=list(maxiter=500))

plot(prop~dev.pm, data=logis.dat)
points(x=dev.pm, y=predict(nls.delta.pm), col="red")

delta_logis_pm  <- as.numeric(coef(nls.delta.pm))[1]
prop_logis_pm  <- as.numeric(coef(nls.delta.pm))[2]

# Input parameters for estimation

# Specify Initial Values for Model Parameters =======================
# Initialization Function
init_fn <- function(chain_id=1) {
  list( 
    "scale" = runif(1, 1e3, 3e4),
    "scale_hist" = runif(nYearPM, 1e4, 3e4),
    "muDay" = rnorm(1,prior.muDay.mean,1),
    "sigmaDay" = rnorm(1,prior.sigmaDay.mean,1),
    "muDay_hist" = rnorm(nYearPM,prior.muDay.mean,1),
    "sigmaDay_hist" = rnorm(nYearPM,prior.sigmaDay.mean,1),
    "sigmaOE"=runif(1,0.5,1),
    "sigmaOE_hist"=runif(nYearPM,0.5,1),
    "prop_logis_pm"=runif(1,quantile(ce.props$prop.bay$prop, probs=0.25), 
                            quantile(ce.props$prop.bay$prop, probs=0.25))
    
    # "prop_ce_adj" = rnorm(1, 0, 0.001),
    # "ln_delta_logis" = log(runif(1,0.1,0.2))
    # "int_ce_pm" = runif(1, 50,75),
    # "slp_ce_pm" = runif(1,1,3)
  )
}
# init_fn()
# Initial List of Lists for Multiple Chains
init_ll <- lapply(1:n.chains, function(id) init_fn(chain_id = id))
# init_ll <- lapply(1, function(id) init_fn(chain_id = id))

# Generate Outputs Necessary for Compendium ========================
# Replicates ADMB model outputs

# admb.out

# Fit Stan Model ===================================================
#Fit the model
stan.fit <- NULL
# if(fit==TRUE) {
stan.fit <- stan(file=file.path(dir.stan, paste0("Bay-", version, ".stan")),
                 model_name=paste0("Bay-", version),
                 data=list("sigma_pf"=sigma.pf, "mu_pf"=mu.pf, "curr_pf"=curr.pf,
                           "curr_ce"=curr.ce, "hist_ce"=hist.ce,
                           
                           "prior_ce_alpha"=prior.ce.alpha, "prior_ce_beta"=prior.ce.beta,
                           
                           "n_curr_pm_days"=n.curr.pm.days,  "curr_pm_days"=curr.pm.days,
                           "curr_cpue_daily_obs"=curr.cpue.daily.obs, "hist_cpue_daily_obs"=hist.cpue.daily.obs,
                           
                           
                           "yearPF"=yearPF, "nYearPF"=nYearPF,
                           "yearCE"=yearCE, "nYearCE"=nYearCE,
                           "yearPM"=yearPM, "nYearPM"=nYearPM,
                           
                           "Robs_ce"=Robs.ce, "Robs_pm"=Robs.pm, 
                           
                           "Robs_pm_age"=Robs.pm.age,
                           "n_ages"=n.ages,
                           "curr_age_cpue_obs"=curr.age.cpue.obs, 
                           "hist_age_cpue_obs"=hist.age.cpue.obs,
                           "pred_PMslope"=pred_PMslope, "sigma_PMslope"=sigma_PMslope,
                           
                           "prior_muDay_mean"=prior.muDay.mean, "prior_muDay_sd"=prior.muDay.sd,
                           "prior_sigmaDay_mean"=prior.sigmaDay.mean, "prior_sigmaDay_sd"=prior.sigmaDay.sd,
                           
                           "n_pm_days"=out.cpue$n.pm.days, "pm_days"=out.cpue$pm.days,
                           
                           # "n_ce_days"=ce.props$n.ce.days, 
                           # "loc_ce_days"=ce.props$loc.ce.days, "bay_days_props"=ce.props$bay.days.props,
                           
                           "obs_ce_time"=obs_ce_time, "obs_ce_time_mean"=mean(obs_ce_time),
                           "obs_pm_time"=obs_pm_time, "obs_ce_prop"=logis.dat$prop,
                           "avg_prop_bay"=ce.props$avg.prop.bay,
                           "int_ce_pm"=as.numeric(coef(lm.timing)[1]), "slp_ce_pm"=as.numeric(coef(lm.timing)[2])
                           # "delta_logis_pm"=delta_logis_pm, "prop_logis_pm"=prop_logis_pm
                 ),
                 chains=n.chains, iter=n.iter, thin=n.thin,
                 # chains=3, iter=5e3, thin=5,
                 cores=n.chains, verbose=FALSE,
                 seed=101,
                 control = list(adapt_delta = 0.99),
                 init=init_ll)
beep(2)
stan.fit
pars <- rstan::extract(stan.fit)
# hist(pars$curr_ce_time_dev)

# Sampling
sum.df <- data.frame(summary(stan.fit)$summary)
# hist(sum.df$n_eff)
# hist(sum.df$Rhat)
# 
# View(sum.df)

hist(pars$curr_pm_time_dev)
hist(pars$curr_ce_time_dev)

hist(pars$curr_prop_ce)
summary(pars$curr_prop_ce)
summary(pars$pred_ce)
summary(pars$pred_pm)

# Logistic estimation
hist(pars$delta_logis)
hist(pars$prop_logis)
hist(pars$sigma_logis)

# Plot Predicted Relationship
plot(x=obs_pm_time-mean(obs_pm_time), y=logis.dat$prop)
obs_pm_time_dev <- obs_pm_time-mean(obs_pm_time)
temp.ord <- order(obs_pm_time_dev)
lines(x=obs_pm_time_dev[temp.ord], y=apply(pars$pred_ce_prop, 2, mean)[temp.ord], col=rgb(1,0,0, alpha=0.5), lwd=2)
abline(v=mean(pars$curr_pm_time_dev))


# ShinyStan ========================================================

# require(shinystan)
# launch_shinystan(stan.fit)

# Output: Plots ====================================================


# Run Size for Given Year
if(myYear <= max(brood.dat$retYr)) {
  obs.rs.ce <- sum(ce.data$number[ce.data$year==myYear])
  obs.rs.bt <- sum(brood.dat$return[brood.dat$retYr==myYear])
}else {
  obs.rs.ce <- NA
  obs.rs.bt <- NA
}

x.lim <- c(min(pars$pred, pars$pred_ce, curr.pf, obs.rs.ce, obs.rs.bt, na.rm=TRUE),
           max(pars$pred, pars$pred_ce, curr.pf, obs.rs.ce, obs.rs.bt, na.rm=TRUE))

par(mfcol=c(3,2), oma=c(3,1,3,1), mar=c(2,4,1,1))

# Total Prediction
hist(pars$pred, freq=FALSE, main="", xlab="", ylab="Total Pred", col='lightgray', 
     breaks=50, xlim=x.lim)
abline(v=curr.pf, col=rgb(1,0,0, alpha=0.5), lwd=2, lty=1)
abline(v=obs.rs.ce, col=rgb(0,0.5,0, alpha=0.5), lwd=2, lty=1)
abline(v=obs.rs.bt, col=rgb(0,0,1, alpha=0.5), lwd=2, lty=1)

legend('topleft', legend=c("Preseason Fcst.","Obs. CE","Obs. BT"), lty=c(1,1,1),
       col=c(rgb(1,0,0, alpha=0.5),
             rgb(0,0.5,0, alpha=0.5),
             rgb(0,0,1, alpha=0.5)))

# CE Proportion
hist(pars$curr_prop_ce, freq=FALSE, main="", xlab="", ylab="CE Proportion", col='lightgray',
     breaks=50, xlim=c(0,1))

# CE Prediction
hist(pars$pred_ce, freq=FALSE, main="", xlab="", ylab="C+E Pred", col='lightgray',
     breaks=50, xlim=x.lim)
abline(v=curr.pf, col=rgb(1,0,0, alpha=0.5), lwd=2, lty=1)
abline(v=obs.rs.ce, col=rgb(0,0.5,0, alpha=0.5), lwd=2, lty=1)
abline(v=obs.rs.bt, col=rgb(0,0,1, alpha=0.5), lwd=2, lty=1)

# Refined Column ================

x.lim <- c(20e3, 90e3)

# Total Prediction
hist(pars$pred, freq=FALSE, main="", xlab="", ylab="Total Pred", col='lightgray', 
     breaks=50, xlim=x.lim)
abline(v=curr.pf, col=rgb(1,0,0, alpha=0.5), lwd=2, lty=1)
abline(v=obs.rs.ce, col=rgb(0,0.5,0, alpha=0.5), lwd=2, lty=1)
abline(v=obs.rs.bt, col=rgb(0,0,1, alpha=0.5), lwd=2, lty=1)

legend('topleft', legend=c("Preseason Fcst.","Obs. CE","Obs. BT"), lty=c(1,1,1),
       col=c(rgb(1,0,0, alpha=0.5),
             rgb(0,0.5,0, alpha=0.5),
             rgb(0,0,1, alpha=0.5)))


# CE Proportion
hist(pars$curr_prop_ce, freq=FALSE, main="", xlab="", ylab="CE Proportion", col='lightgray',
     breaks=50, xlim=c(0,1))

# CE Prediction
hist(pars$pred_ce, freq=FALSE, main="", xlab="", ylab="C+E Pred", col='lightgray',
     breaks=50, xlim=x.lim)
abline(v=curr.pf, col=rgb(1,0,0, alpha=0.5), lwd=2, lty=1)
abline(v=obs.rs.ce, col=rgb(0,0.5,0, alpha=0.5), lwd=2, lty=1)
abline(v=obs.rs.bt, col=rgb(0,0,1, alpha=0.5), lwd=2, lty=1)


mtext(paste(myDay, "of", myYear), side=3, outer=TRUE)
mtext("Run Size (thousands)", side=1, outer=TRUE)


# Create List of Posterior Predictions ================================================
preds.mat <- data.frame(pars$pred, pars$post_pred_pf, pars$post_pred_pm, pars$post_pred_ce)
names(preds.mat) <- c("Combined", "Preseason Forecast", "Port Moller", "C+E")
preds.list <- preds.mat %>% gather()
names(preds.list) <- c("Forecast","value")
preds.list$Forecast <- factor(preds.list$Forecast)
preds.list$Forecast <- preds.list$Forecast %>% forcats::fct_relevel(c("Preseason Forecast", "Port Moller", "C+E", "Combined"))

g <- ggplot(preds.list, aes(x=Forecast, y=value/1e3, fill=Forecast)) +
  theme_linedraw() +
  scale_fill_colorblind() +
  stat_eye(.width=c(0.5,0.95), alpha=0.75) +
  ylab("Run Size (millions)") +
  ggtitle(as.Date(myDay-1, origin = paste0(myYear,"-01-01"))) +
  geom_hline(yintercept = curr.pf/1e3, color='red') +
  geom_hline(yintercept = obs.rs.ce/1e3, color='dark green') +
  geom_hline(yintercept = obs.rs.bt/1e3, color='blue') +
  coord_flip(ylim=c(0, 100)) +
  theme(legend.position = "top")
# geom_density()
g      
ggsave(file.path(dir.figs, paste(myYear, myDay, "Eye.pdf")), height=3, width=5, units="in")

g.2 <- ggplot(preds.list, aes(value/1e3, fill=Forecast)) +
  theme_linedraw() +
  scale_fill_colorblind() +
  geom_density(alpha=0.5) +
  xlab("Run Size (millions)") +
  ylab("Relative Probability Density") +
  ggtitle(as.Date(myDay-1, origin = paste0(myYear,"-01-01"))) +
  geom_vline(xintercept = curr.pf/1e3, color='red') +
  geom_vline(xintercept = obs.rs.ce/1e3, color='dark green') +
  geom_vline(xintercept = obs.rs.bt/1e3, color='blue') +
  theme(legend.position='bottom')

g.2 
ggsave(file.path(dir.figs, paste(myYear, myDay, "Distribution_2.pdf")), height=5, width=5, units="in")
g.3 <- g.2 +coord_cartesian(xlim=c(10,100))
g.3
ggsave(file.path(dir.figs, paste(myYear, myDay, "Distribution_1.pdf")), height=5, width=5, units="in")

# Plot: Cumulative Distribution
g.cum <- ggplot(preds.list, aes(value/1e3, color=Forecast)) +
  theme_linedraw() +
  scale_color_colorblind() +
  geom_hline(yintercept=0.5) +
  geom_hline(yintercept=c(0.25,0.75), lty=2) +
  stat_ecdf(lwd=2, alpha=0.7) +
  facet_wrap(~Forecast, ncol=1, scales="free_x") +
  theme(legend.position='none') +
  xlab("Run Size (millions)") +
  ylab("Probability of a Run Size < X") +
  ggtitle(as.Date(myDay-1, origin = paste0(myYear,"-01-01")))
g.cum
ggsave(file.path(dir.figs, paste(myYear, myDay, "Cumulative.pdf")), height=7, width=4, units="in")
g.cum.2 <- g.cum + facet_wrap(~Forecast, ncol=1) + coord_cartesian(xlim=c(10,120))
ggsave(file.path(dir.figs, paste(myYear, myDay, "Cumulative_std.pdf")), plot=g.cum.2, height=7, width=4, units="in")

# Plots: Forecast Variances =======================================

df.sigmas <- data.frame(as.numeric(pars$sigma_pm), as.numeric(pars$sigma_ce))
names(df.sigmas) <- c("Port Moller", "C+E")
list.sigmas <- melt(df.sigmas)

# Weights
names.sds <- c("Preseason Forecast", "Port Moller", "C+E")
sds <- c(sigma.pf, mean(pars$sigma_pm), mean(pars$sigma_ce))
inv.var <- 1/sds^2
weights <- inv.var/sum(inv.var)
names(weights) <- names.sds
weights


# g <- ggplot(list.sigmas, aes(x=factor(variable), y=value)) +
# theme_linedraw() +
# geom_eyeh()
# g
# Plots: RPI =============================================  
df.rpi <- data.frame(as.matrix(pars$rpi_age))
names(df.rpi) <- ages
list.rpi <- melt(df.rpi)
names(list.rpi) <- c("Age","value")
g.rpi <- ggplot(list.rpi, aes(value, fill=Age)) +
  theme_linedraw() +
  scale_fill_colorblind() +
  geom_density(alpha=0.5) +
  ggtitle("Run Per Index (RPI)")
# coord_cartesian(xlim=c(0,1000))
g.rpi
ggsave(file.path(dir.figs, paste(myYear, myDay, "RPI.pdf")), height=5, width=5, units="in")

# Plots: C+E Ratio =============================================
prop.est <- data.frame("Posterior", pars$curr_prop_ce)
prop.prior <- rbeta(nrow(prop.est), prior.ce.alpha, prior.ce.beta)
prop.prior <- data.frame("Prior", prop.prior)
names(prop.est) <- c("Distribution","value"); names(prop.prior) <- c("Distribution","value")
prop.comb <- rbind(prop.prior, prop.est)

g.prop <- ggplot(prop.comb, aes(x=value, fill=Distribution)) +
            theme_linedraw() +
            scale_fill_colorblind() +
            geom_density(alpha=0.3) +
            ggtitle("CE Ratio")
            
g.prop
ggsave(file.path(dir.figs, paste(myYear, myDay, "CE Ratio.pdf")), plot=g.prop, height=5, width=5, units="in")

# Plot Fits ========================================================
# CE
# pars$hist_pred_ce
# ce.years

# PM

# Output: Tables ===================================================
# loo::waic(extract_log_lik(stan.fit))


head(summary(stan.fit)$summary)

# % Difference from Median
print(paste("Current Forecast (median) of:", round(median(pars$pred),3),
            "is", round((median(pars$pred)-curr.pf)/curr.pf*100,2), 
            "above/below the preseason forecast of:",curr.pf))

print(paste("Current Forecast (mean) of:", round(mean(pars$pred),3),
            "is", round((mean(pars$pred)-curr.pf)/curr.pf*100,2), 
            "above/below the preseason forecast of:",curr.pf))

# shinystan::launch_shinystan(stan.fit)

# Empirical Cumulative Distribution ============
#  Calculate probabilities that the run is less than a threshold
temp.ecdf <- ecdf(preds.list$value[preds.list$Forecast=="Combined"])
print(paste("Probability that run is LESS than 30-million:",temp.ecdf(30e3)))
print(paste("Probability that run is LESS than 40-million:",temp.ecdf(40e3)))
print(paste("Probability that run is LESS than 50-million:",temp.ecdf(50e3)))
print(paste("Probability that run is LESS than 60-million:",temp.ecdf(60e3)))
print(paste("Probability that run is LESS than 70-million:",temp.ecdf(70e3)))
print(paste("Probability that run is LESS than 80-million:",temp.ecdf(80e3)))
dev.off()

# Plots: Plot Relationship Between PM timing Dev and C+E Proportion ============

par(mfrow=c(1,1), mar=c(4,4,4,1))

plot(x=obs_pm_time-mean(obs_pm_time), y=logis.dat$prop,
       xlab="Port Moller Timing", ylab="Proportion of Annual Run Observed",
       type="p", pch=21, bg=rgb(0,0,1, alpha=0.5),
       main="Bristol Bay")
# Determine order
obs_pm_time_dev <- obs_pm_time-mean(obs_pm_time)
temp.ord <- order(obs_pm_time_dev)

pred.prop.quants <- apply(pars$pred_ce_prop, 2, quantile, probs=c(0.025,0.25,0.5,0.75,0.975))
polygon(x=c(obs_pm_time_dev[temp.ord], rev(obs_pm_time_dev[temp.ord])), # 95%
          y=c(pred.prop.quants[1,temp.ord], rev(pred.prop.quants[5,temp.ord])), 
          col=rgb(1,0,0, 0.25), border=FALSE)
polygon(x=c(obs_pm_time_dev[temp.ord], rev(obs_pm_time_dev[temp.ord])), # 50%
        y=c(pred.prop.quants[2,temp.ord], rev(pred.prop.quants[4,temp.ord])), 
        col=rgb(1,0,0, 0.25), border=FALSE)
lines(x=obs_pm_time_dev[temp.ord], y=pred.prop.quants[3,temp.ord], 
        col=rgb(1,0,0, 0.5), lwd=2)
# Current prediction
segments(x0=median(pars$curr_pm_time_dev), y0=-100, x1=median(pars$curr_pm_time_dev), y1=median(pars$curr_prop_ce),
           lty=3, lwd=3, col="darkgreen")
segments(x0=-100, y0=median(pars$curr_prop_ce), x1=median(pars$curr_pm_time_dev), y1=median(pars$curr_prop_ce),
         lty=3, lwd=3, col="darkgreen")
points(x=median(pars$curr_pm_time_dev), y=median(pars$curr_prop_ce), pch=21, bg="darkgreen")
legend("topright", legend=c(paste("Pt. Moller Timing:", round(median(pars$curr_pm_time_dev),2), "days"),
                            paste0("(",round(quantile(pars$curr_pm_time_dev,probs=0.025),2)," - ",
                                       round(quantile(pars$curr_pm_time_dev,probs=0.975),2),") days"),
                            paste("Prop. of Run Observed:", round(median(pars$curr_prop_ce)*100,1), "%"),
                            paste0("(",round(quantile(pars$curr_prop_ce,probs=0.025)*100,1)," - ",
                                   round(quantile(pars$curr_prop_ce,probs=0.975)*100,1),"%)")),
       title="Current Estimates")


# Plots: Predicted Port Moller Distribution ====================================
str(pars$pred_cpue_daily)

hist(pars$muDay)
hist(pars$sigmaDay)
hist(pars$scale)
hist(pars$sigmaOE)

# Limits
x.lim <- c(161,201)
y.lim <- c(0,max(apply(pars$pred_cpue_daily, 2, quantile, probs=0.975), curr.cpue.daily.obs))

# Plot Observed
plot(x=curr.pm.days, y=curr.cpue.daily.obs, type='p', pch=21, col='gray', xlim=x.lim, ylim=y.lim,
     xlab='', ylab='',
     main="Bristol Bay: Current Year", xaxt='n')

axis(side=1, at=seq(from=151, to=206, by=5), labels = FALSE, col='darkgray')
axis(side=1, at=seq(from=161, to=201, by=10),
     labels=c("June-10","June-20","June-30","July-10","July-20"))
segments(x0=curr.pm.days, y0=rep(0, length(curr.pm.days)), x1=curr.pm.days, y1=curr.cpue.daily.obs, col='black', lwd=2)
points(x=curr.pm.days, y=curr.cpue.daily.obs, type='p', pch=21, bg='gray')

# Plot Predicted
polygon(x=c(pm.days,rev(pm.days)), y=c(apply(pars$pred_cpue_daily, 2, quantile, probs=0.025),
                                 rev(apply(pars$pred_cpue_daily, 2, quantile, probs=0.975))), border=FALSE, col=rgb(1,0,0,0.2))
polygon(x=c(pm.days,rev(pm.days)), y=c(apply(pars$pred_cpue_daily, 2, quantile, probs=0.25),
                                 rev(apply(pars$pred_cpue_daily, 2, quantile, probs=0.75))), border=FALSE, col=rgb(1,0,0,0.2))
lines(x=pm.days, y=apply(pars$pred_cpue_daily, 2, median), col=rgb(1,0,0, alpha=0.5), lwd=3)
abline(v=prior.muDay.mean, col='blue', lty=2, lwd=2)

# Labels
mtext(paste(myDay,'of',myYear), side=3, outer=TRUE, line=0.5, font=2)
mtext('Day of Year', side=1, outer=TRUE, line=0.5, font=2)
mtext('UW-FRI Port Moller Index', side=2, outer=TRUE, line=0.5, font=2)
legend('topleft', legend=c('Obs.','Pred.', 'Avg. Peak'), col=c('black','red','blue'), lty=c(1,1,2))

# Historical
y <- 14
for(y in 1:nYearPM) {
  year <- yearPM[y]
  
  # Limits
  x.lim <- c(161,201)
  y.lim <- c(0,max(apply(pars$pred_cpue_daily_hist[,,y], 2, quantile, probs=0.975), curr.cpue.daily.obs))
  
  # Plot Observed
  plot(x=curr.pm.days, y=hist.cpue.daily.obs[,y], type='p', pch=21, col='gray', xlim=x.lim, ylim=y.lim,
       xlab='', ylab='',
       main=year, xaxt='n')
  
  axis(side=1, at=seq(from=151, to=206, by=5), labels = FALSE, col='darkgray')
  axis(side=1, at=seq(from=161, to=201, by=10),
       labels=c("June-10","June-20","June-30","July-10","July-20"))
  segments(x0=curr.pm.days, y0=rep(0, length(curr.pm.days)), x1=curr.pm.days, y1=hist.cpue.daily.obs[,y], col='black', lwd=2)
  points(x=curr.pm.days, y=hist.cpue.daily.obs[,y], type='p', pch=21, bg='gray')
  
  # Plot Predicted
  polygon(x=c(pm.days,rev(pm.days)), y=c(apply(pars$pred_cpue_daily_hist[,,y], 2, quantile, probs=0.025),
                                         rev(apply(pars$pred_cpue_daily_hist[,,y], 2, quantile, probs=0.975))), border=FALSE, col=rgb(1,0,0,0.2))
  polygon(x=c(pm.days,rev(pm.days)), y=c(apply(pars$pred_cpue_daily_hist[,,y], 2, quantile, probs=0.25),
                                         rev(apply(pars$pred_cpue_daily_hist[,,y], 2, quantile, probs=0.75))), border=FALSE, col=rgb(1,0,0,0.2))
  lines(x=pm.days, y=apply(pars$pred_cpue_daily_hist[,,y], 2, median), col=rgb(1,0,0, alpha=0.5), lwd=3)
  abline(v=prior.muDay.mean, col='blue', lty=2, lwd=2)
  
  # Labels
  mtext(paste(myDay,'of',myYear), side=3, outer=TRUE, line=0.5, font=2)
  mtext('Day of Year', side=1, outer=TRUE, line=0.5, font=2)
  mtext('UW-FRI Port Moller Index', side=2, outer=TRUE, line=0.5, font=2)
  legend('topleft', legend=c('Obs.','Pred.', 'Avg. Peak'), col=c('black','red','blue'), lty=c(1,1,2))
}

# Compare Predictions with Posterior Predictions =====================


summary(pars$pred_ce)
# summary(pars$post_pred_ce)
# 
# summary(pars$pred_pm)
# summary(pars$post_pred_pm)
# 
# summary(pars$pred_pf)
# summary(pars$post_pred_pf)
# 
# 
# summary(pars$pred)
# summary(pars$post_pred_pf)


# 

