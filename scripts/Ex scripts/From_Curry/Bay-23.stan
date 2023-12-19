// BASIC INSEASON MODEL
// NOTES:
//  a) _lpdf signals it's the log probability density function
//  b) When left side of a ~ is not a data object, we need to update the posterior density.
//       target += normal_lpdf(y | mu, sigma); INSTEAD OF y ~ normal(mu, sigma); - Which would have the same effect if y was data.

// Components:
//  1) Catch and escapement predictor
//  2) Traditional Flynn Forecast
//  3) Preseason Forecast as Prior
//  4) Port Moller Slope Forecast
//  5) Port Moller Norm Fit Run Timing

  
  // Preseason Forecast
  // C+E Forecast
  // Flynn Forecast  
  // PM Slope Forecast
  // PM Timing


data {
  // int<lower=0> n_dists; //Number of Districts
  // int dists[n_dists];  //Names of Districts
  
  // Preseason Forecast
  int nYearPF;
  int yearPF[nYearPF];
  
  real sigma_pf;
  real mu_pf;
  real curr_pf;
  
  // C+E Forecast
  int nYearCE;
  int yearCE[nYearCE];
  vector[nYearCE] Robs_ce;
  
  real prior_ce_alpha;
  real prior_ce_beta;
  
  real curr_ce;
  vector[nYearCE] hist_ce;
  
  // Flynn Forecast
  int nYearPM;
  int yearPM[nYearPM];
  vector[nYearPM] Robs_pm;
  
  int n_curr_pm_days;
  vector[n_curr_pm_days] curr_pm_days;
  vector[n_curr_pm_days] curr_cpue_daily_obs;
  matrix[n_curr_pm_days,nYearPM] hist_cpue_daily_obs;
  
  int n_ages;
  matrix[nYearPM, n_ages] Robs_pm_age;
  matrix[n_curr_pm_days, n_ages] curr_age_cpue_obs;
  real hist_age_cpue_obs[n_curr_pm_days, n_ages, nYearPM];
  
  // PM Slope Forecast
  real pred_PMslope;
  real sigma_PMslope;
  
  // PM Timing
  real prior_muDay_mean; 
  real prior_muDay_sd;
  real prior_sigmaDay_mean;
  real prior_sigmaDay_sd;
  
  int n_pm_days;
  vector[n_pm_days] pm_days;
  
  // int n_ce_days;
  // int loc_ce_days;
  // vector[n_ce_days] bay_days_props;
  
  vector[nYearPM] obs_ce_time;
  real obs_ce_time_mean;
  vector[nYearPM] obs_pm_time;
  vector[nYearPM] obs_ce_prop;
  real avg_prop_bay;
  
  // Relationship between PM peak and CE peak (complex version)
  real int_ce_pm;
  real slp_ce_pm;

  // Relationship between CE proportion and PM peak ()
  // real delta_logis_pm;
  // real prop_logis_pm;
  
}

parameters {
  real<lower=0> pred;
  
  // Preseason Forecast
  // C+E Forecast
  // real<lower=0, upper=1> prop_ce;
  // real int_ce;
  // real<lower=0> sigma_ce;
  
  // Flynn Forecast
  // real<lower=0> rpi;
  // real rpi_intercept;
  real<lower=0> sigma_pm;
  real<lower=0> rpi_age[n_ages];
  real int_pm[n_ages];
  real<lower=0> sigma_pm_age[n_ages];
  
  // PM Timing
  real<lower=161, upper=201> muDay; 
  real<lower=1, upper=15> sigmaDay;
  real<lower=0, upper=1e5> scale;
  real<lower=0> sigmaOE;
  
  real<lower=161, upper=201> muDay_hist[nYearPM];
  real<lower=1, upper=15> sigmaDay_hist[nYearPM];
  real<lower=0, upper=1e5> scale_hist[nYearPM];
  real<lower=0> sigmaOE_hist[nYearPM];
  
  // Estimating PM Peak - CE Median Relationship
  real<lower=0, upper=10> delta_logis_pm;
  real<lower=0, upper=1> prop_logis_pm;
  real<lower=0> sigma_logis;
  // real ln_delta_logis;
  
    // real<lower=1e-3> int_ce_pm;
  // real<lower=1e-3> slp_ce_pm;
  // real<lower=1e-3> sigma_ce_pm;
  
}

transformed parameters {
  // C+E Forecast
  real pred_ce;
  vector[nYearCE] hist_pred_ce;
  real<lower=0> sigma_ce;  //Could specify as empirical
  
  // Flynn Forecast
  // real sum_curr_cpue;
  // vector[nYearPM] sum_hist_cpue;
  real pred_pm;
  vector[nYearPM] hist_pred_pm;
  
  vector[n_ages] sum_curr_cpue_age;
  matrix[nYearPM, n_ages] sum_hist_cpue_age;
  // real<lower=0> sum_hist_cpue_age[nYearPM, n_ages];
  vector[n_ages] pred_pm_age;
  matrix[nYearPM, n_ages] hist_pred_pm_age;
  
  // PM Timing
  vector[n_pm_days] pred_cpue_daily;
  vector[n_curr_pm_days] pred_cpue_daily_obs;
  matrix[n_pm_days, nYearPM] pred_cpue_daily_hist;
  matrix[n_curr_pm_days, nYearPM] pred_cpue_daily_obs_hist;
  
  // Convert PM timing to C+E timing
  real curr_ce_time;
  real curr_ce_time_dev;
  
  // PM Timing deviation
  real curr_pm_time_dev;
  vector[nYearPM] hist_pm_time_dev;
 
  // Predicted C+E proportion
  real<lower=0, upper=1> curr_prop_ce;
  vector<lower=0, upper=1>[nYearPM] hist_prop_ce;
  
  // Predicted CE proportion
  vector<lower=0, upper=1>[nYearPM] pred_ce_prop;
  vector[nYearPM] obs_pm_time_dev;
  
  for(a in 1:n_ages) {
    sum_curr_cpue_age[a] = sum(curr_age_cpue_obs[1:n_curr_pm_days,a]);
    pred_pm_age[a] = sum_curr_cpue_age[a] * rpi_age[a] + int_pm[a];
    for(y in 1:nYearPM) {
      sum_hist_cpue_age[y,a] = sum(hist_age_cpue_obs[1:n_curr_pm_days,a,y]);
      hist_pred_pm_age[y,a] = sum_hist_cpue_age[y,a] * rpi_age[a] + int_pm[a];
    }
  }
  pred_pm = sum(pred_pm_age);
  for(y in 1:nYearPM) {
    hist_pred_pm[y] = sum(hist_pred_pm_age[y,1:n_ages]);
  }
  
  // PM Timing - Predict CPUE
  pred_cpue_daily[1:n_pm_days] = scale * (1/(sigmaDay*sqrt(2*pi())))*
                                    exp(-0.5*square((pm_days-muDay)/sigmaDay));
  // pred_cpue_daily[1:n_pm_days] = scale * (pm_days| muDay, sigmaDay);                                  
                                    
  pred_cpue_daily_obs[1:n_curr_pm_days] = scale * (1/(sigmaDay*sqrt(2*pi())))*
                                    exp(-0.5*square((curr_pm_days-muDay)/sigmaDay));
                                    
                                    
  for(y in 1:nYearPM) {
    for(t in 1:n_pm_days) {
      pred_cpue_daily_hist[t,y] = scale_hist[y] * (1/(sigmaDay_hist[y]*sqrt(2*pi())))*
                                    exp(-0.5*square((pm_days[t]-muDay_hist[y])/sigmaDay_hist[y]));
    }
    for(t in 1:n_curr_pm_days) {
      pred_cpue_daily_obs_hist[t,y] = scale_hist[y] * (1/(sigmaDay_hist[y]*sqrt(2*pi())))*
                                    exp(-0.5*square((curr_pm_days[t]-muDay_hist[y])/sigmaDay_hist[y]));
    }
  } // next y
  
  // Generate Prediction for Current Timing
  curr_ce_time = int_ce_pm + slp_ce_pm*muDay;
  curr_ce_time_dev = curr_ce_time - obs_ce_time_mean;
  
  // Calculate Port Moller Timing Deviation
  curr_pm_time_dev = muDay - prior_muDay_mean; // Run timing deviation based on Port Moller
  curr_prop_ce = 1/(1+exp(-delta_logis_pm*((log((1-prop_logis_pm)/prop_logis_pm+1e-3)/
                   (-delta_logis_pm))-curr_pm_time_dev))); // Timing-adjusted Predicted proportion
  pred_ce = curr_ce/(curr_prop_ce + 1e-6); // Current run size prediction
  
  for(y in 1:nYearPM) {
    hist_pm_time_dev[y] = muDay_hist[y] - prior_muDay_mean; // Run timing deviation based on Port Moller
    hist_prop_ce[y] = 1/(1+exp(-delta_logis_pm*((log((1-prop_logis_pm)/prop_logis_pm)/
                        (-delta_logis_pm))-hist_pm_time_dev[y]))); // Timing-adjusted Predicted proportion
    hist_pred_ce[y] = hist_ce[y]/(hist_prop_ce[y] + 1e-6); // Current run size prediction
  }
  
  // Predicted Relationship Between observed and predicted proportions
  for(y in 1:nYearPM) {
    obs_pm_time_dev[y] = obs_pm_time[y] - mean(obs_pm_time);
    pred_ce_prop[y] = 1/(1+exp(-delta_logis_pm*((log((1-prop_logis_pm)/prop_logis_pm)/
                        (-delta_logis_pm))-obs_pm_time_dev[y])));
  }
  
  // Empirical CE sigma
  sigma_ce = sd( log( Robs_ce ./ hist_pred_ce + 1e-6) );
  
  // Updated Run Timing Prediction
  // adj = -1*curr_ce_time_dev;
  // comp_day = loc_ce_days + adj;// + 1; #To get correct reference
  // int_comp_day = trunc(comp_day);
  // diff_comp_day = comp_day-int_comp_day;
  // 
  // prop_t = bay_days_props[int_comp_day];
  // prop_tplus = bay_days_props[int_comp_day+1];
  // 
  // 
  // est_prop = prop_t + (prop_tplus - prop_t)*diff_comp_day; #
  // est_runSize = (1/est_prop)*curr_ce;
}

model {
  // PRIORS
  pred ~ normal(0,1e5);  //Annual Run Size Prediction
  // pred ~ normal(mean(Robs_ce), sd(Robs_ce)); // Empirical prior
  // prop_ce ~ beta(prior_ce_alpha, prior_ce_beta); // Estimated proportion of CE observed by this date
  // prop_ce ~ beta(1,1); 
  // sigma_ce ~ normal(0,5);
  // int_ce ~ normal(0,1e4);
  
  sigma_pm ~ normal(0,5);
  

  // for(a in 1:n_ages) {
  //   rpi_age[a] ~ normal(0,1e4); // Could be drawn from MVN norm
  //   sigma_pm_age[a] ~ normal(0,5);
  //   int_pm[a] ~ normal(0,1e4);
  // }
  
  // Vectorize
  rpi_age ~ normal(0,1e4); // Could be drawn from MVN norm
  sigma_pm_age ~ normal(0,5);
  int_pm ~ normal(0,1e4);
  
  // PM Timing
  muDay ~ normal(prior_muDay_mean, prior_muDay_sd);
  sigmaDay ~ normal(prior_sigmaDay_mean, prior_sigmaDay_sd);
  scale ~ uniform(1e2,1e5);
  sigmaOE ~ normal(0,1);
  
  muDay_hist ~ normal(prior_muDay_mean, prior_muDay_sd);
  sigmaDay_hist ~ normal(prior_sigmaDay_mean, prior_sigmaDay_sd);
  scale_hist ~ uniform(1e2,1e5);
  sigmaOE_hist ~ normal(0,1);
  
  // Estimating PM Peak - CE Median Relationship
  // int_ce_pm ~ normal(0,100);
  // slp_ce_pm ~ normal(0,5);
  // sigma_ce_pm ~ uniform(1e-3,1e1);
  
  // Estimating PM Peak - CE Median Relationship
  delta_logis_pm ~ uniform(0,10);
  prop_logis_pm ~ uniform(0,1);
  sigma_logis ~ normal(0,1);
  
  // Preseason Forecast
  // log(curr_pf) ~ normal(log(pred+1e-6)-((sigma_pf^2)/2), sigma_pf);
  log(curr_pf) ~ normal(log(pred+1e-6), sigma_pf);
  
  // C+E Forecast
  // for(y in 1:nYearCE) {  // Likelihood for Past Years
  // //   // log(Robs_ce[y]) ~ normal(log(hist_pred_ce[y]+1e-6)-((sigma_ce^2)/2), sigma_ce);
  //   log(Robs_ce[y]) ~ normal(log(hist_pred_ce[y]+1e-6), sigma_ce); //Was current version
  // //   // Robs_ce[y] ~ normal(hist_pred_ce[y], sigma_ce); ///TESTING ONLY
  // }
  
  // Vectorize
  // log(Robs_ce) ~ normal(log(hist_pred_ce+1e-6), sigma_ce); // Empirical calculation above now.
  
  // log(pred_ce) ~ normal(log(pred)-((sigma_ce^2)/2), sigma_ce); // Likelihood for current year
  // target += normal_lpdf(log(pred_ce+1e-6) | log(pred+1e-6)-((sigma_ce^2)/2), sigma_ce);
  target += normal_lpdf(log(pred_ce+1e-6) | log(pred+1e-6), sigma_ce); //Was current version
  
  
  // Flynn Forecast
  // for(y in 1:nYearPM) { // Likelihood for total (across ages) PM prediction
  //   // log(Robs_pm[y]) ~ normal(log(hist_pred_pm[y]+1e-6)-((sigma_pm^2)/2) , sigma_pm);
  //   log(Robs_pm[y]) ~ normal(log(hist_pred_pm[y]+1e-6), sigma_pm);
  // }
  
  // Vectorize
  log(Robs_pm) ~ normal(log(hist_pred_pm+1e-6), sigma_pm);
  
  // target += normal_lpdf(log(pred_pm+1e-6) | log(pred+1e-6)-((sigma_pm^2)/2), sigma_pm); // Likelihood for current forecast - ERROR NOT HERE
    target += normal_lpdf(log(pred_pm+1e-6) | log(pred+1e-6), sigma_pm); // Likelihood for current forecast - ERROR NOT HERE
  
  for(a in 1:n_ages) { // Likelihood for age-specific PM fits
    for(y in 1:nYearPM) {
      // log(Robs_pm_age[y,a]) ~ normal(log(hist_pred_pm_age[y,a]+1e-6)-((sigma_pm_age[a]^2)/2) , sigma_pm_age[a]);
      log(Robs_pm_age[y,a]) ~ normal(log(hist_pred_pm_age[y,a]+1e-6) , sigma_pm_age[a]); //Error in location parameter
    }
  }
  
  // PM Slope Forecast
  // log(curr_pf) ~ normal(log(pred+1e-6)-((sigma_PMslope^2)/2), sigma_PMslope);
  // log(pred_PMslope) ~ normal(log(pred+1e-6), sigma_PMslope); // Turned off in v22
  
  // PM Timing - Predict CPUE
  for(t in 1:n_curr_pm_days) {
    log(curr_cpue_daily_obs[t]+1e-6) ~ normal(log(pred_cpue_daily_obs[t]+1e-6), sigmaOE);
    // curr_cpue_daily_obs[t] ~ normal(pred_cpue_daily_obs[t], sigmaOE); //TESTING
    
    for(y in 1:nYearPM) {
      log(hist_cpue_daily_obs[t,y]+1e-6) ~ normal(log(pred_cpue_daily_obs_hist[t,y]+1e-6), sigmaOE_hist[y]);
      // hist_cpue_daily_obs[t,y] ~ normal(pred_cpue_daily_obs_hist[t,y], sigmaOE_hist[y]); //TESTING
    } // next (y) Port Moller year
  } //  next (t) day with current port moller data
  
  // Estimating Logistic Parameters
  // for(y in 1:nYearPM) {
  //   obs_ce_prop[y] ~ normal(pred_ce_prop[y], sigma_logis);
  // }
  
  // Vectorize
  obs_ce_prop ~ normal(pred_ce_prop, sigma_logis); // Should likely be beta
  
  // Estimating PM Peak - CE Median Relationship
  // for(y in 1:nYearPM) {
  //   obs_ce_time[y] ~ normal(pred_ce_time[y], sigma_ce_pm);
  // } //next y
}

generated quantities {
  real post_pred_pf;
  real post_pred_pm;
  real post_pred_ce;
  // real post_pred_PMslope;
  
  // real curr_ce_time_post;
  // real curr_ce_time_dev_post;
  // vector[nYearPM] hist_ce_time_post;
  // vector[nYearPM] hist_ce_time_dev_post;

  // Posterior Predictive Distributions
  
  // Preseason Forecast
  // post_pred_pf = exp(normal_rng(log(curr_pf+1e-6)-((sigma_pf^2)/2), sigma_pf));
  post_pred_pf = exp(normal_rng(log(curr_pf+1e-6), sigma_pf));

  // Flynn Forecast
  // post_pred_pm = exp(normal_rng(log(pred_pm+1e-6)-((sigma_pm^2)/2), sigma_pm));
  post_pred_pm = exp(normal_rng(log(pred_pm+1e-6), sigma_pm));

  // C+E Forecast
  // post_pred_ce = exp(normal_rng(log(pred_ce+1e-6)-((sigma_ce^2)/2), sigma_ce));
  post_pred_ce = exp(normal_rng(log(pred_ce+1e-6), sigma_ce));
  
  // PM Slope Forecast
  // post_pred_PMslope = exp(normal_rng(log(pred_PMslope+1e-6)-((sigma_PMslope^2)/2), sigma_PMslope));
  // post_pred_PMslope = exp(normal_rng(log(pred_PMslope+1e-6), sigma_PMslope));
  
  // PM Timing: Generate Prediction for Current Timing
  // curr_ce_time_post = normal_rng(curr_ce_time, sigma_ce_pm);
  // curr_ce_time_dev_post = curr_ce_time_post - mean(obs_ce_time);

  // for(y in 1:nYearPM) {
    // hist_ce_time_post[y] = normal_rng(hist_ce_time[y], sigma_ce_pm);
    // hist_ce_time_dev_post[y] = hist_ce_time_post[y] - mean(obs_ce_time);
  // } // next y
  
}





 

