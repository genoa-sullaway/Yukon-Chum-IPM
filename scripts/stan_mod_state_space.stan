data {
  int<lower=0> A;           // Number of age classes
  int<lower=0> C;           // Number of cohorts
  int<lower=0> Y;           // Number of years
  int<lower=0> a_min;       // Minimum age
  int<lower=0> a_max;       // Maximum age
  matrix[Y, A] x;           // Age counts
  
  real<lower=0> data_h_b[Y];   // data harvest below weir
  real<lower=0> data_w[Y];    // data weir passage = escapement
  real<lower=0> data_h_a[Y];  // data harvest above weir 
  
  real<lower=0> cv_hb[Y];   // Coefficient of variation for harvest below weir
  real<lower=0> cv_w[Y];    // Coefficient of variation for weir passage
  real<lower=0> cv_ha[Y];   // Coefficient of variation for harvest above weir
  real<lower=0> first_brood_year;  // First brood year
}

parameters {
  real lnalpha; // ricker productivity parameter 
  real beta; // ricker capacity (inverse) parameter 
  real phi; // AR1 coefficient
  real<lower=0> sigma_R; // standard deviation of individual normally distributed process errors for Ey
 // real<lower=0> mean_log_R0; // I may not need this, this is for  draw before age structure was available
 // real<lower=0> tau_R0; // I may not need this, this is the draw before age structure was available
  real log_resid_0; // residuals at time step 0 ? or mean residualas 
  real<lower=0> D_scale;
  real<lower=0> pi_2p; // expected proportions at each age
  real<lower=0> pi_3p; // expected proportions at each age
  real<lower=0> pi_4p; // expected proportions at each age
  vector<lower=0>[A] g[Y];
  real mu_HB[Y];
  real mu_HA[Y];
//  real mu[Y];
 
}

transformed parameters {
  real alpha = exp(lnalpha);
 // real sigma_R0 = 1 / sqrt(tau_R0);
 // real tau_red = tau_R * (1 - phi * phi);
  real lnalpha_c = lnalpha + (sigma_R * sigma_R / 2 / (1 - phi * phi));
  real S_msr = 1 / beta;
  real R_0[A];
  real gamma[A];
  real pi_a[A];
  real p[Y, A];
  real N_ya[Y, A];
  real N[Y];
  real q[Y, A];
  
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
 
// Dirichlet (approximated with gamma and division) maturity schedule by cohort
  D = 1 / (D_scale * D_scale);
 
  pi_a[2] = pi_a[2] * (1 - pi_a[1]);
  pi_a[3] = pi_a[3] * (1 - pi_a[1] - pi_a[2]);
  pi_a[4] = pi_a[4] * (1 - pi_a[1] - pi_a[2] - pi_a[3]);
  pi_a[5] = 1 - pi_a[1] - pi_a[2] - pi_a[3] - pi_a[4]; 
 
  for (a in 1:A) {
    gamma[a] = D * pi_a[a]; //This parameter governs the variability of the age proportion vectors across cohorts, smaller D = more variability 
    for (c in 1:C) {                                                    
      //g[c,a] ~ gamma(gamma[a],0.1); // independent gamma variates Eq 5 Fleischman
      p[c,a] <- g[c,a]/sum(g[c,]); // age at maturity proportions Eq 4 Fleischman
      }
  }
      
// N_ya total spawners by age class, a product of p*r [ages] (y index starts at 1st year of escapement data) 
  for (a in 1:A) {
    for (y in a:(Y + (a - 1))) {
      N_ya[y - (a - 1), (A + 1 - a)] = p[y, (A + 1 - a)]*R[y]; // eq 3 
      // R is below - line 130 - this component need to be mvoed around, line 130 probs needs to be moved up to transformed parameters 
    }
  }
  
// Multinomial age counts observed
// "Variance estimates of age proportions from the time-stratified design were used to obtain annual “effective sample sizes” nEy, 
    // where nEy is the multinomial sample size that would produce uncertainty equivalent to that indicated by the time-stratified analysis
  for (y in 1:Y) {
    N[y] = sum(N_ya[y, ]); // eq 7 - The total run abundance in calendar year y was the sum of abundance-at-age across all ages:
    for (a in 1:A) {
      q[y, a] = N_ya[y, a] / N[y];
    }
    n[y] <- sum(x[y,1:A]); // multinomial sample size 
    x[y,1:A] ~ multi_normal()(q[y,],n[y]); // eq 13
    }
  }
  
// calculating spawners, fish that reach the weir and harvest - eq 8-11
  for (y in 1:Y) {
    H_B[y] = mu_HB[y] * N[y];
    W[y] = max(N[y] - H_B[y], 1);
    H_A[y] = mu_HA[y] * W[y];
    S[y] = max(W[y] - H_A[y], 1);
  }
}

model {
  // priors 
  lnalpha ~ normal(0, 1.0E-6);
  beta ~ normal(0, 1.0E-6);
  phi ~ normal(0, 1.0E-6);
  sigma_R ~ uniform(0, 100);
 // mean_log_R0 ~ normal(0, 1.0E-12);
 // tau_R0 ~ gamma(0.001, 0.001);
  log_resid_0 ~ normal(0, tau_red);
  D_scale ~ uniform(0, 1);
  g[C,A]; // independent gamma variates Eq 5 Fleischman
  
  for (y in 1:Y) {
  mu_HB[y] ~ beta(0.5,0.5);
  mu_HA[y] ~ beta(0.5,0.5); 
  }

  //for age proprtions 
  pi_a[1] ~ beta(0.2,0.8)
  pi_2p ~ beta(0.2,0.6)
  pi_3p ~ beta(0.2,0.4)
  pi_4p ~ beta(0.2,0.2)
   
  for (c in 1:C) {                                                    
      g[c,a] ~ gamma(gamma[a],0.1); // independent gamma variates Eq 5 Fleischman
    }
  
  for (c in a_min:C) {
    log_R[c] ~ normal(log_R_mean2[c], tau_R);
     R[c] = exp(log_R[c]);
    log_R_mean1[c] = log(S[c - a_max]) + lnalpha - beta * S[c - a_max]; 
    log_resid[c] = log(R[c]) - log_R_mean1[c]; // eq 2
  }
  log_R_mean2[a_min] = log_R_mean1[a_min] + phi * log_resid_0;
  
  for (c in a_min + 1:C) {
    log_R_mean2[c] = log_R_mean1[c] + phi * log_resid[c - 1]; // eq 1 

  // Liklilihoods  
for(k in 1:K){
  for (i in 2:N) {
   // data_log_stage_j[i,k] ~ normal(log(N_j[i,k]), sigma_y_j[k]);
    // data_log_stage_sp[i,k] ~ normal(log(N_sp[i,k]), sigma_y_sp[k]);
    
    data_w ~ normal(log.W[y],tau.log.w[y])  
    data_h_b  ~ normal(log.HB[y],tau.log.hb[y])
    data_h_a  ~ normal(log.HA[y],tau.log.ha[y])

    } 
  }
} 


  }
  