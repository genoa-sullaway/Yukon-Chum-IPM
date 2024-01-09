data { // all equation references are from proposal numbering
  int<lower=0> N ; // total observations 
  int<lower=0> K; // number of stocks
  real<lower=0> Ps; // Proportion of females in spawning stock, based on lit
  int<lower=0> fs; // Fecundity of each female in each stock - eventually extend for age ?
  //matrix[N,K] data_stage_r; // vector of number of juveniles for each group  (basis)
  matrix[N,K] data_stage_j; // vector of number of juveniles for each group  (basis)
  matrix[N,K] data_stage_sp;// vector of number of spawners for each group (escapement)
 
  real <lower=0>kappa_sp_start[K]; // adding starting values for kappa so there arent NAs..not sure if this is necessary
  real <lower=0>kappa_j_start[K];
  
  int<lower=0> ncovars1; //number of covariates for first lifestage
  int<lower=0> ncovars2; //number of covariates for second lifestage 

  real cov1[N, ncovars1]; // covariate data in a matrix format 
  real cov2[N, ncovars2]; 
  
  real <lower=0> basal_p_1[K]; // mean survival absent of density dependence - for now just add it in dont estimate it. 
  real <lower=0> basal_p_2[K];
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
  real<lower=0>sigma_y_j[K];
  real<lower=0>sigma_y_sp[K];
    
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
real kappa_sp[N,K]; // predicted survival for each stock
// 
real cov_eff1[N, K, ncovars1];
real cov_eff2[N, K, ncovars2];

  for (k in 1:K) {
  kappa_sp[1,k] = kappa_sp_start[k]; 
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


for(k in 1:K){  // loop for each population
  for (i in 2:N){ //will need to add a loop in here for stocks too..
    N_e[i,k] = fs*Ps*N_sp[i-1,k]; // Eq 4.3 generated estimate for the amount of eggs produced that year for that stock.
    
    kappa_j[i,k] =  p_1[i,k]/ (1 + ((p_1[i,k]*N_e[i,k])/c_1[k])); // Eq 4.1  - Bev holt transition estimating survival from Egg to Juvenile (plugs into Eq 4.4) 
    
    N_j[i,k] = kappa_j[i,k]*N_e[i,k]; // Eq 4.4  generated estiamte for the amount of fish each year and stock that survive to a juvenile stage
   
    kappa_sp[i,k] =  p_2[i,k]/ (1 + ((p_2[i,k]*N_j[i,k])/c_2[k])); // Eq 4.1   - Bev holt transition estimating survival from juvenile to spawner (plugs into Eq 4.4) 
   
    N_sp[i,k] = kappa_sp[i,k]*N_j[i,k]; // Eq 4.5 generated estiamte for the amount of fish each year and stock that survive to a spawning stage
   }
 }
}

model {
  // PRIORS
for(k in 1:K) { 
   sigma_y_j[k] ~  normal(1,10);
   sigma_y_sp[k] ~ normal(0,10);
   
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
    
    c_2[1] ~ normal(1e4, 1e5); // magic number right now: 1e4, 1e5  
    c_2[2] ~ normal(1e4, 1e5); 
    c_2[3] ~ normal(1e4, 1e5);
             
// Liklilihoods -- 
for(k in 1:K){
  for (i in 2:N) {
    data_log_stage_j[i,k] ~ normal(log(N_j[i,k]), sigma_y_j[k]);
    data_log_stage_sp[i,k] ~ normal(log(N_sp[i,k]), sigma_y_sp[k]);
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

