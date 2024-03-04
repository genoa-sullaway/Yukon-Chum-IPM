data { 
  int<lower=0> A;           // Number of age classes
  int<lower=0> C;           // Number of cohorts
  int<lower=0> Y;           // Number of years
  int nRyrs;                // Number of recruitment years  
  int<lower=0> a_min;       // Minimum age
  int<lower=0> a_max;       // Maximum age
  matrix[Y, A] x;           // Age counts
  
  matrix<lower=0>[Y, A] o_run;        // Observed run size by age class
  matrix<lower=0, upper=1>[Y, A] o_run_comp; // Observed age composition by year
  
  real<lower=0> data_h_b[Y];       // data harvest below weir
  real<lower=0> data_w[Y];         // data weir passage = escapement
  real<lower=0> data_h_a[Y];       // data harvest above weir 
  
  real<lower=0> cv_hb[Y];          // Coefficient of variation for harvest below weir
  real<lower=0> cv_w[Y];           // Coefficient of variation for weir passage
  real<lower=0> cv_ha[Y];          // Coefficient of variation for harvest above weir
  real<lower=0> first_brood_year;  // First brood year
  vector [Y] ess_age_comp;         // Effective input sample size for age comp observations
  
}

parameters { 
  
  vector<lower=0>[nRyrs] lnR;   // CC log recruitment states
  real lnalpha;                 // ricker productivity parameter 
  real beta;                    // ricker capacity (inverse) parameter 
  real<lower=-1,upper=1> phi;   // lag-1 correlation in process error
  real<lower=0> sigma_R;        // standard deviation of individual normally distributed process errors for Ey
  real<lower=0> sigma_R0;       // Process error for first a.max years with no spawner link
 
 // real<lower=0> mean_log_R0; // I may not need this, this is for  draw before age structure was available
 // real<lower=0> tau_R0; // I may not need this, this is the draw before age structure was available
  
  real log_resid_0;      // residuals at time step 0 ? or mean residualas 
  real<lower=0> D_scale; // Variability of age proportion vectors across cohorts

  vector<lower=0,upper=1>[3] prob;  // Maturity schedule probs -- from CC
  
  vector<lower=0>[A] g[Y]; // from CC:  matrix<lower=0.01> [nRyrs, A] g;    // Individual year/age class gamma variates for generating age at maturity proportions
  real mu_HB[Y];
  real mu_HA[Y]; 

/// params from CC
  real lnresid_0;              // First residual for lag-1 correlation in process error
  real<lower=0> mean_ln_R0;    // "true" mean log recruitment in first a.max years with no spawner link

}

transformed parameters {
//  transformed params ============ 
  vector[Y] lnS;              // log spawner states
  vector<lower=0>[nRyrs] R;   // Recruitment states
  
  real<lower=0> D_sum;                   // Inverse of D_scale which governs variability of age proportion vectors across cohorts
  vector<lower=0>[A] Dir_alpha;          // Dirichlet shape parameter for gamma distribution used to generate vector of age-at-maturity proportions
  matrix<lower=0, upper=1>[Y, A] q;      // Age composition by year/age classr matrix
  
  vector[nRyrs] lnresid;                 // log recruitment residuals
  vector[nRyrs] lnRm_1;                  // log recruitment states in absence of lag-one correlation
  vector[nRyrs] lnRm_2;                  // log recruitment states after accounting for lag-one correlation
  
  // from CC
  // I dont think I need this, this is observed and I think they had it because they had a full run reconstruction, but I have post-RR information... 
  // matrix<lower=0>[Y, A] o_run;        // Observed run size by age class
  // matrix<lower=0, upper=1>[Y, A] o_run_comp; // Observed age composition by year
  
  real alpha;
  real lnalpha_c; // not sure if needed

 // real sigma_R0 = 1 / sqrt(tau_R0);
 // real tau_red = tau_R * (1 - phi * phi);
  real S_msr = 1 / beta;
  real R_0[A];
  real gamma[A];
  real pi[A];
  real p[Y, A];
  real N_ya[Y, A];
  real N[Y];
  
  real HB[Y];
  real log_HB[Y];
  real tau_log_hb[Y];
  
  real W[Y];
  real log_W[Y]; 
  real tau_log_w[Y];
     
  real HA[Y];
  real log_HA[Y]; 
  real tau_log_ha[Y];
   
  real S[Y]; 
  real mu[Y]; 
  real D; 

// do I need this?? -- I think I do because it provides S... and CC gets S from RR.  
// HARVEST BELOW WEIR, WEIR PASSAGE, AND HARVEST ABOVE WEIR OBSERVED
  for (y in 1:Y) {
    HB[y] = mu_HB[y] * N[y];
    log_HB[y] = log(HB[y]);
    tau_log_hb[y] = 1 / log(cv_hb[y]*cv_hb[y] + 1); // Eq 12 
   
    W[y] = fmax(N[y] - HB[y], 1);
    log_W[y] = log(W[y]); 
    tau_log_w[y] = 1 / log(cv_w[y]*cv_w[y] + 1); 
 
    HA[y] = mu_HA[y] * W[y];
    log_HA[y] = log(HA[y]);
    tau_log_ha[y] = 1 / log(cv_ha[y]*cv_ha[y] + 1);
   
    S[y] = fmax(W[y] - HA[y], 1); 
    mu[y] = (HB[y] + HA[y]) / N[y]; 
    }
    
  // Calculate ln spawners 
  for(t in 1:Y) {
    lnS[t] = log(S[t]); 
   }
     
// Maturity ====
  // from CC: 
// Maturity schedule: use a common maturation schedule to draw the brood year specific schedules
  pi[1] = prob[1];
  pi[2] = prob[2] * (1 - pi[1]);
  pi[3] = prob[3] * (1 - pi[1] - pi[2]);
  pi[4] = 1 - pi[1] - pi[2] - pi[3];
  D_sum = 1/D_scale^2;

  for (a in 1:A) {
    Dir_alpha[a] = D_sum * pi[a];
    for (c in 1:C) {
      p[c,a] = g[c,a]/sum(g[c,]);
    }
  }
  
   // Total spawners by age class, N_ya  ====
   // Calculate the numbers at age matrix as brood year recruits at age (proportion that matured that year)
  for (y in 1:Y) {
    for(a in 1:A){
      N_ya[y,a] = R[y+A-a] * p[y+A-a,a]; // fleishman eq 3 
    }
  }
  
  R = exp(lnR);
  
// OLD adapted from fleishman code: OLD
  // D = 1 / (D_scale * D_scale);
  // 
  // pi_a[2] = pi_a[2] * (1 - pi_a[1]);
  // pi_a[3] = pi_a[3] * (1 - pi_a[1] - pi_a[2]);
  // pi_a[4] = pi_a[4] * (1 - pi_a[1] - pi_a[2] - pi_a[3]);
  // pi_a[5] = 1 - pi_a[1] - pi_a[2] - pi_a[3] - pi_a[4]; 
  // 
  // for (a in 1:A) {
  //   gamma[a] = D * pi_a[a]; //This parameter governs the variability of the age proportion vectors across cohorts, smaller D = more variability 
  //   for (c in 1:C) {                                                    
  //     //g[c,a] ~ gamma(gamma[a],0.1); // independent gamma variates Eq 5 Fleischman
  //     p[c,a] <- g[c,a]/sum(g[c,]); // age at maturity proportions Eq 4 Fleischman
  //     }
  // }
      
  // Calculate age proportions by return year - CC had simpler code than fleishman, so adapted below
  for (y in 1:Y) {
    for(a in 1:A){
      q[y,a] = N_ya[y,a]/sum(N_ya[y,1:A]); // eq 7 - The total run abundance in calendar year y was the sum of abundance-at-age across all ages
    }
  }
  
//  OLD - 
// Multinomial age counts observed
// "Variance estimates of age proportions from the time-stratified design were used to obtain annual “effective sample sizes” nEy, 
  // where nEy is the multinomial sample size that would produce uncertainty equivalent to that indicated by the time-stratified analysis
  // mostly same as above but I am not sure where eq 13 fits into either model really, not quite sure what it does and if I need it .... 
// for (y in 1:Y) {
  //   N[y] = sum(N_ya[y, ]); // eq 7 - The total run abundance in calendar year y was the sum of abundance-at-age across all ages:
  //   for (a in 1:A) {
  //     q[y, a] = N_ya[y, a] / N[y];
  //   }
  //   n[y] <- sum(x[y,1:A]); // multinomial sample size 
  //   x[y,1:A] ~ multi_normal()(q[y,],n[y]); // eq 13
  //   }
  // }
  
    // not sure if I need this ... from fleishmanm 
    // for (c in 1:C) {                                                    
    //   g[c,a] ~ gamma(gamma[a],0.1); // independent gamma variates Eq 5 Fleischman
    // }
  // Ricker SR with AR1 process on log recruitment residuals for years with brood year spawners
  for (i in 1:nRyrs) {
    lnresid[i] = 0.0;
    lnRm_1[i] = 0.0;
    lnRm_2[i] = 0.0;
  }

 
alpha = exp(lnalpha);
lnalpha_c = lnalpha + (sigma_R * sigma_R / 2 / (1 - phi * phi));


  // added from CC
    for (y in (C+a_min):nRyrs) {
    lnRm_1[y] = lnS[y-a_max] + lnalpha - beta * S[y-a_max]; 
    lnresid[y] = lnR[y] - lnRm_1[y];
  }

  lnRm_2[A+a_min] =  lnRm_1[A+a_min] + phi * lnresid_0;

  for (y in (A+a_min+1):nRyrs) {
    lnRm_2[y] =  lnRm_1[y] + phi * lnresid[y-1];
  }
  
  // old convereted from fleishman 
  // for (c in a_min:C) {
  //   log_R_mean1[c] = log(S[c - a_max]) + lnalpha - beta * S[c - a_max]; 
  //   log_resid[c] = log(R[c]) - log_R_mean1[c]; // eq 2
  // }
  // log_R_mean2[a_min] = log_R_mean1[a_min] + phi * log_resid_0;
  // 
  // for (c in a_min + 1:C) {
  //   log_R_mean2[c] = log_R_mean1[c] + phi * log_resid[c - 1]; // eq 1 

  // Observed age proportions by return year
  // for(y in 1:Y){
  //   for(a in 1:A){
  //     o_run[y,a] = (H_comps_bp[y,a]*eh_low[1, y]) + (H_comps_ap[y,a]*eh_up[1, y]) + (S_comps[y,a]*canpass[y]); // run by age
  //   } // next a
  //   o_run_comp[y,] = o_run[y,]/(sum(o_run[y,])+1e-3);// run comps by age
  // } // next y
  
  
// calculating spawners, fish that reach the weir and harvest - eq 8-11
  for (y in 1:Y) {
    HB[y] = mu_HB[y] * N[y];
    W[y] = fmax(N[y] - HB[y], 1);
    HA[y] = mu_HA[y] * W[y];
    S[y] = fmax(W[y] - HA[y], 1);
  }
}

model {
  // priors 
  lnalpha ~ normal(0,3);
  beta ~ normal(0,1);
  sigma_R ~ normal(0,2);
  lnresid_0 ~ normal(0,20);
  mean_ln_R0 ~ normal(0,20);
  sigma_R0 ~ inv_gamma(2,1); // made this an informative prior based on metanalysis of other AK chinook stocks (Fleischman et al. 2013), less informative priors resulted in divergent tranistions
  prob[1] ~ beta(1,1);
  prob[2] ~ beta(1,1);
  prob[3] ~ beta(1,1);
  D_scale ~ beta(1,1);
  sigma_run ~ normal(0,20);
  
  g[C,A]; // independent gamma variates Eq 5 Fleischman
  
  for (y in 1:Y) {
  mu_HB[y] ~ beta(0.5,0.5);
  mu_HA[y] ~ beta(0.5,0.5); 
  }
 
  // for (c in 1:C) {                                                    
  //     g[c,a] ~ gamma(gamma[a],0.1); // independent gamma variates Eq 5 Fleischman
  //   }
  // 
  // for (c in a_min:C) {
  //   log_R[c] ~ normal(log_R_mean2[c], tau_R);
  //    R[c] = exp(log_R[c]);
  //   log_R_mean1[c] = log(S[c - a_max]) + lnalpha - beta * S[c - a_max]; 
  //   log_resid[c] = log(R[c]) - log_R_mean1[c]; // eq 2
  // }
  // log_R_mean2[a_min] = log_R_mean1[a_min] + phi * log_resid_0;
  // 
  // for (c in a_min + 1:C) {
  //   log_R_mean2[c] = log_R_mean1[c] + phi * log_resid[c - 1]; // eq 1 

  // Liklilihoods 
    
  // Gamma variates for each year and age class which are used to determine age at maturity proportions
  for (y in 1:nRyrs) {
    for (a in 1:A) {
      //g[y,a] ~ gamma(Dir_alpha[a],1);
      target += gamma_lpdf(g[y,a]|Dir_alpha[a],1);
    }
  }
  
  // this is a  likelihood from flieshman - 
  for(y in 1:Y){
    data_w ~ normal(log.W[y],tau.log.w[y])  
    data_h_b  ~ normal(log.HB[y],tau.log.hb[y])
    data_h_a  ~ normal(log.HA[y],tau.log.ha[y])
    } 
 
 // liklihood from Curry
  // First `a.max` years of recruits, for which there is no spawner link
  lnR[1:a_max] ~ normal(mean_ln_R0, sigma_R0);

  // State model
  lnR[(A+a_min):nRyrs] ~ normal(lnRm_2[(A+a_min):nRyrs], sigma_R);

  // Observation model
  for(t in 1:n_year){
    if(sum(S_comps[t])>0){
    target += ess_age_comp[t]*sum(o_run_comp[t,1:A] .* log(q[t,1:A])); // time varying ESS for age comp likelihood for ONLY years with data
     }
    target += normal_lpdf(log(run[t]) | log(sum(N_ta[t,1:A])), sigma_run); // Im not sure if I need this, I think this was used for CC when they were comparing the RR with SRA
  }
 }
}
  
generated quantities {
  real<lower=0> S_max;      // Spawner abundance that produces maximum recruitment
  real<lower=0> S_eq;       // Equilibrium spawner abundance
  vector[nRyrs] lnalpha_y;  // Time trend in intrinsic productivity
  real<lower=0> lnalpha_c;  // Log-normal bias corrected log alpha
  real<lower=0> S_eq_c;     // Log-normal bias corrected equilibrium spawner abundance
  
  S_max = 1/beta;
  S_eq = lnalpha * S_max;
  lnalpha_y = lnalpha + lnresid;
  lnalpha_c = lnalpha + (sigma_R * sigma_R)/2/(1-phi * phi);
  S_eq_c = lnalpha_c * S_max;
}

