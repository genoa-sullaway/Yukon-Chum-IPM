data { // all equation references are from proposal numbering
  int<lower=0> N ; // total observations 
  int<lower=0> K; // number of stocks
  real<lower=0> Ps; // Proportion of females in spawning stock, based on lit
  int<lower=0> fs; // Fecundity of each female in each stock - eventually extend for age ?
  // matrix[N,K] data_stage_r; // vector of number of juveniles for each group  (basis)
  matrix[N,K] data_stage_j;    // vector of number of juveniles for each group  (basis)
  matrix[N,K] data_stage_sp;   // vector of number of spawners for each group (escapement)
  real<lower=0>sigma_y_j[K];   // Initially fix sigma 
  real<lower=0>sigma_y_sp[K];  // Initially fix sigma 
  
  real <lower=0>kappa_marine_start[K]; // adding starting values for kappa so there arent NAs..not sure if this is necessary
  real <lower=0>kappa_j_start[K];
  
  int<lower=0> ncovars1; //number of covariates for first lifestage
  int<lower=0> ncovars2; //number of covariates for second lifestage 

  real cov1[N, ncovars1]; // covariate data in a matrix format 
  real cov2[N, ncovars2]; 
  
  real <lower=0> basal_p_1[K]; // mean survival absent of density dependence - for now just add it in dont estimate it. 
  real <lower=0> basal_p_2[K];
  
  // age comps 
  int A;                            // Number of (total) age classes
  vector<lower=0,upper=1>[4] prob;  // Maturity schedule probs - currently data, eventually will probably be random effect, 4 long because of 4 age classes
  matrix<lower=0>[N, A] o_run;      // Observed run size by age class
  matrix<lower=0, upper=1>[N, A] o_run_comp; // Observed age composition by year
  vector [N] ess_age_comp;   // Effective input sample size for age comp "observations"
  
}
  
transformed data {
// seed initial population dynamics 
  matrix[N,K] data_log_stage_j;
  matrix[N,K] data_log_stage_sp;
 // matrix[N,K] data_log_stage_r;

//data_log_stage_r = log(data_stage_r);
data_log_stage_j = log(data_stage_j); // Log transform data 
data_log_stage_sp = log(data_stage_sp);

}

parameters {
// real<lower=0>sigma_y_j[K];
//  real<lower=0>sigma_y_sp[K];
    
  real<lower=0>c_1[K]; // carrying capacity 
  real<lower=0>c_2[K]; // carrying capacity 
  
  // covariate parameters 
  real theta1[K, ncovars1]; // covariate estimated for each covariate and each population 
  real theta2[K,ncovars2];
  
  real mu_coef1[ncovars1]; // mean covariate effect across populations (group level hierarchical effect)
  real mu_coef2[ncovars2];
  
  real sigma_coef1[ncovars1]; // error of the covariate effect across populations (group level hierarchical effect)
  real sigma_coef2[ncovars2]; 
  
  real log_N_sp_start[K];
  real log_N_egg_start[K];
  real log_N_j_start[K];
  
}

transformed parameters { 
matrix[N,K] N_e; // predicted eggs, basically a dummy step. 
matrix[N,K] N_j; // predicted juveniles - this goes into the liklihood, data involved 
matrix[N,K] N_sp;
matrix[N,K] p_1;
matrix[N,K] p_2;

real N_sp_start[K];
real N_egg_start[K];
real N_j_start[K]; 
  
real kappa_j[N,K]; // predicted survival for each stock
real kappa_marine[N,K]; // predicted survival for each stock
 
real cov_eff1[N, K, ncovars1];
real cov_eff2[N, K, ncovars2];

// Age related transformed params ====== 
matrix<lower=0, upper=1>[N, A] p;  // Age at maturity proportions
vector<lower=0,upper=1>[4] pi;     // Maturity schedule probs
real<lower=0> D_sum; // Inverse of D_scale which governs variability of age proportion vectors across cohorts
matrix<lower=0, upper=1>[N, A] q;   // Age composition by year/age classr matrix

  for (k in 1:K) {
  kappa_marine[1,k] = kappa_marine_start[k]; 
  kappa_j[1,k]= kappa_j_start[k]; 
 
  N_sp_start[k] = exp(log_N_sp_start[k]); // transform predicted spawners
  N_egg_start[k] = exp(log_N_egg_start[k]); // transform predicted eggs 
  N_j_start[k] = exp(log_N_j_start[k]); // transform predicted juveniles

  N_sp[1,k] = N_sp_start[k];
  N_e[1,k] = N_egg_start[k];
  N_j[1,k] = N_j_start[k]; 
    }
   
// the cov effects need seperate loop because number of covariates varies between lifestage
   for (k in 1:K){
   for(i in 1:N){
   for (c in 1:ncovars1) {
  cov_eff1[i,k,c] = theta1[k,c]*cov1[i,c];
   }
  }
}

for (k in 1:K){
    for(i in 1:N){
     for (c in 1:ncovars2) {
  cov_eff2[i,k,c] = theta2[k,c]*cov2[i,c];
   }
  }
}
   
 for(c in 1:ncovars1){
  for (k in 1:K){
    for (i in 1:N) {
     p_1[i,k]  = 1 / exp(-basal_p_1[k]-sum(cov_eff1[i,k,1:c]));// sum(theta1[k,]*cov1[i,])); # estimate theta for each popualtion and each covariate, not every year 
   }
  }
 }
 
for(c in 1:ncovars2){
  for (k in 1:K){
    for (i in 1:N) { 
     p_2[i,k]  = 1 / exp(-basal_p_2[k] -sum(cov_eff2[i,k,1:c]));
   }
  }
 }

 // Maturity schedule: use a common maturation schedule to draw the brood year specific schedules
 // currently fixing this but will likely eventually have it as a random variable.  
  pi[1] = prob[1]; // because its fixed, prob is currently data
  pi[2] = prob[2] * (1 - pi[1]);
  pi[3] = prob[3] * (1 - pi[1] - pi[2]);
  pi[4] = 1 - pi[1] - pi[2] - pi[3];
  D_sum = 1/D_scale^2;

  for (a in 1:A) {
    Dir_alpha[a] = D_sum * pi[a];
    for (y in 1:nRyrs) {
      p[y,a] = g[y,a]/sum(g[y,]);
    }
  }

  // Calculate the numbers at age matrix as brood year recruits at age (proportion that matured that year)
  for (t in 1:n_year) {
    for(a in 1:A){
      N_ta[t,a] = R[t+A-a] * p[t+A-a,a];
    }
  }

// i dont know if this needs to be looped or not. 
for(k in 1:K){  // loop for each population
  for (i in 2:N){ //will need to add a loop in here for stocks too..
    N_e[i,k] = fs*Ps*N_sp[i-1,k]; // Eq 4.3 generated estimate for the amount of eggs produced that year for that stock.
    
    kappa_j[i,k] =  p_1[i,k]/ (1 + ((p_1[i,k]*N_e[i,k])/c_1[k])); // Eq 4.1  - Bev holt transition estimating survival from Egg to Juvenile (plugs into Eq 4.4) 
    
    N_j[i,k] = kappa_j[i,k]*N_e[i,k]; // Eq 4.4  generated estiamte for the amount of fish each year and stock that survive to a juvenile stage
   
    kappa_marine[i,k] =  p_2[i,k]/ (1 + ((p_2[i,k]*N_j[i,k])/c_2[k])); // Eq 4.1   - Bev holt transition estimating survival from juvenile to spawner (plugs into Eq 4.4) 
   
    N_recruit[i,k] = kappa_marine[i,k]*N_j[i,k]; // Eq 4.5 generated estiamte for the amount of fish each year and stock that survive to a spawning stage
    
    N_returning[i,k] = N_recruit[i,k]*p[t-a,a] // need to figure out the indexing here, maybe needs another loop for a.... 
  
    N_sp[i,k] = N_returning[i,k] - H_b[i,k]    
   }
 }
 
  // Calculate the numbers at age matrix as brood year recruits at age (proportion that matured that year)
  // THis is from currys code, same as a few lines up in my code -- can I have them seperate not all looped in one?? 
  // for (t in 1:n_year) {
  //   for(a in 1:A){
  //     N_ta[t,a] = R[t+A-a] * p[t+A-a,a];
  //   }
  // }

  // Calculate age proportions by return year
  for (t in 1:N) {
    for(a in 1:A){
      q[t,a] = N_returning[t,a]/sum(N_returning[t,1:A]);
    }
  }
}

model {
  // PRIORS
for(k in 1:K) { 
  //start off with sigma fixed
   // sigma_y_j[k] ~  normal(1,10);// if i can get uncertainty from run reconstruction then i can fix this and not estiamte it 
   // sigma_y_sp[k] ~ normal(0,10);
   
   log_N_egg_start[k] ~ normal(18,10);
   log_N_j_start[k] ~ normal(10,10);
   log_N_sp_start[k] ~ normal(14,10); 
}

 for(c in 1:ncovars1){
  	 mu_coef1[c] ~ normal(0, 10);  
     sigma_coef1[c] ~ normal(0, 10);  
   }
for(c in 1:ncovars2){
  	 mu_coef2[c] ~ normal(0, 10); // 0 - 1 or 0.1 would be a penalized version as a test case - drive it to 0
     sigma_coef2[c] ~ normal(0, 10); //0 - 1 or 0.1 would be a penalized version
   }
  	 
for(k in 1:K) { 
   for (c in 1:ncovars1) {
      theta1[k,c] ~ normal(mu_coef1[c],sigma_coef1[c]);
  }
   for (c in 1:ncovars1) {
   theta2[k,c] ~ normal(mu_coef2[c],sigma_coef2[c]);
  }
}
  
    c_1[1] ~ normal(1e6, 1e7); // magic number right now: 1e6, 1e7  
    c_1[2] ~ normal(1e6, 1e7); 
    c_1[3] ~ normal(1e6, 1e7);
    
    c_2[1] ~ normal(1e7, 1e4); // magic number right now: 1e4, 1e5, but this cant be right because yukon reutrns are on scale of 1e6 rogjt  
    c_2[2] ~ normal(1e7, 1e4); 
    c_2[3] ~ normal(1e7, 1e4);
             
 // age comp priors 
  prob[1] ~ beta(1,1);
  prob[2] ~ beta(1,1);
  prob[3] ~ beta(1,1);
  D_scale ~ beta(1,1);

// Liklilihoods -- 

// Observation model
  for(t in 1:n_year){
    if(sum(S_comps[t])>0){
    target += ess_age_comp[t]*sum(o_run_comp[t,1:A] .* log(q[t,1:A])); // time varying ESS for age comp likelihood for ONLY years with data
    }
    target += normal_lpdf(log(data_log_stage_sp[t]) | log(sum(N_sp[t,1:A])), sigma_y_sp);
    
  }

for(k in 1:K){
  for (i in 2:N) {
    data_log_stage_j[i,k] ~ normal(log(N_j[i,k]), sigma_y_j[k]);
  //   data_log_stage_sp[i,k] ~ normal(log(N_sp[i,k]), sigma_y_sp[k]); // i dont think i need this because it is not by age class here and it should be, but juvs dont have an age class so its fine. 
    } 
  }
}  

generated quantities {
  real pp_log_N_j[N,K]; // predicted recruits
  real pp_log_N_sp[N,K]; // predicted spawners
  //real pp_log_N_r[N-1,K];

 for (k in 1:K){
   for (i in 1:N) {
   pp_log_N_j[i,k] = (normal_rng(log(N_j[i,k]) - 0.5 * sigma_y_j[k]^2, sigma_y_j[k])); // generate posterior predictives with backtransform?
   pp_log_N_sp[i,k] = (normal_rng(log(N_sp[i,k]) - 0.5 * sigma_y_sp[k]^2, sigma_y_sp[k])); // generate posterior predictives with backtransform?
    }
 }
}

