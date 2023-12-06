data { // all equation references are from proposal numbering
  int<lower=0> N ; // total observations 
  int<lower=0> K; // number of stocks
  // int<lower=0> ncovars; //number of covariates
  real<lower=0> Ps; // Proportion of females in spawning stock, for now just putting 50
  int<lower=0> fs; // Fecundity of each female in each stock
  
  // stock specific data 
  int<lower=0> N_stock[K];  // Number of data points for each stock
  vector[N] stage_j; // vector of number of juveniles for each group
  vector[N] stage_sp;// vector of number of spawners for each group
  int<lower = 1, upper = K> g[N]; // Vector of group assignments so STAN knows which group it is dealing with in the set of N observations 
 // real covar_1[N, ncovars]; // covariate data note: cannot be coded as matrix 
  }
  

parameters {
  real<lower=0> sigma_y ; // error for each stock
  // Stock specific parameters
  real p_1[K]; // productivity for each stock, calculated based on parameter: average survivial, and later, covariates
  real p_2[K]; // productivity for each stock, calculated based on parameter: average survivial, and later, covariates

  real c_1[K]; // carrying capacity 
  real c_2[K]; // carrying capacity 
  // real avg_B1[K]; // mean survival rate used to estimate p actual survivial rate, later coefficients will be added in here
  // real avg_B2[K]; // mean survival rate used to estimate p actual survivial rate, later coefficients will be added in here
  real<lower=0> stage_j_pred[N]; // juvenile predicted incorporates spawn data from the likelihood [MF included spawner error too]
  real<lower=0> stage_sp_pred[N]; // spawner predicted incorporates spawn  data from the likelihood 

  //COEFFICIENT
 // real theta[K, ncovars]; // covariate estimated for each covariate and each population 
 // real mu_coef[ncovars]; // mean covariate effect across populations (group level hierarchical effect)
 // real<lower=0> sigma_coef[ncovars]; // error of the covariate effect across populations (group level hierarchical effect)
  }
 
transformed parameters { 
real kappa_fw[N]; // predicted survival for each stock
real kappa_sp[N]; // predicted survival for each stock

  for (i in 1:N) {
    kappa_fw[i] =  p_1[g[i]]/ (1 + ((p_1[g[i]]*stage_j_pred[i])/c_1[g[i]])); // Eq 4.1  - Bev holt transition estimating survival from Egg to Juvenile (plugs into Eq 4.4) 
    kappa_sp[i] =  p_2[g[i]]/ (1 + ((p_2[g[i]]*stage_sp_pred[i])/c_2[g[i]])); // Eq 4.1   
  }
  
// real log_stage_fw[N]; // retransformation bias output 
// real log_stage_sp[N]; // retransformation bias output 
 // real log_c_1[K]; // log carrying capacity for each stock
// real log_c_2[K]; // log carrying capacity for each stock

// real avg_B1[K]; I think these should be in parameter section ... 
// real avg_B2[K];
// real cov_eff[N, ncovars]; // overall covariate effect (sum of all covariates for one population)
	
	// 		 for(y in 1:N){
	// 		 	for(c in 1:ncovars){
	// 		  cov_eff[y,c]= theta[g[y],c]*covars[y, c]; // g[y] is the population pointer vector so theta is estimated for each population for each covariate, not every year. See ragged and missing data structure in stan manual for structure
	//  } // next c
	// } //next y
	
// I dont think I need to do log carrying capacity??
  // for (k in 1:K){
  //   c_1[k] = exp(log_c_1[k]); // carrying capacity for each stock
  //   c_2[k] = exp(log_c_2[k]); // carrying capacity for each stock
  // }
    
  // for (i in 1:K) {
  //  p_1[i] = 1 + exp(avg_B1[i]) // initial try without environmental covariates 
  //  p_2[i] = 1 + exp(avg_B2[i]) // initial try without environmental covariates 
  // }
   // p_1[K] = 1 + exp(avg_B + sum(cov_eff[y,1:ncovars])) // Eq 4.2 FW juvenile stage - productivity funtion of average survival rate and sum of environmental covariates
   // p_2[K] = 1 + exp(avg_B + sum(cov_eff[y,1:ncovars])) // Eq 4.2 ocean stage- productivity funtion of average survival rate and sum of environmental covariates
  
}

model {
 
real log_kappa_fw[N]; // predicted log survival for each stock
real log_kappa_sp[N]; // predicted log survival for each stock

   sigma_y ~ cauchy(0,1);
   
for(k in 1:K){
  //avg_B1[k] ~ normal(10,10); // Stan specific loop to assign priors across stocks
   p_1[k] ~ normal(0,1.5^2); //10,10);
   p_2[k] ~ normal(0,1.5^2); //10,10);
   c_1[k] ~ uniform(1000,100000); // normal(1,5); <- these are my old prior values, currys are the uniform values. 
   c_2[k] ~ uniform(1000,100000); // normal(1,5);
}

// Liklilihoods -- see line 98 in megans code
  for (i in 1:N) {
   log_lk_kappa_fw[i] ~ lognormal(log(kappa_fw[i]), sigma_y); // vague lognormal prior
   log_lk_kappa_sp[i] ~ lognormal(log(kappa_sp[i]), sigma_y); // vague lognormal prior
  }
}  
  //  for(c in 1:ncovars){
  // 	// mu_coef[c] ~ normal(0, 10); // 0 - 1 or 0.1 would be a penalized version as a test case - drive it to 0
  //    sigma_coef[c] ~ normal(0, 5); //0 - 1 or 0.1 would be a penalized version
  // 	 }
  	 
//   	  //Covariate Effect priors
//     for(c in 1:ncovars){
// 		 theta[s,c] ~ normal(mu_coef[c],sigma_coef[c]); //  s is the number of stocks ....
//    }

generated quantities {
	real<lower=0> pp_kappa_fw[N]; // predicted survival stage fw to juvenile
	real<lower=0> pp_kappa_sp[N]; // predicted survival stage spawner
	// real<lower=0> E[N]; // Eggs	
	// real<lower=0> N_j[N];	// Number of fish surviving to juvenile
	// real<lower=0> N_sp[N];	// number of fish surviving to spawn

//   real mu_coef_rep[ncovars];
//   // use current estimate of mu_coef to generate new sample
//   for (c in 1:ncovars) {
//     mu_coef_rep[c] = normal_rng(mu_coef[c],sigma_coef[c]);
//    }
 
  for (i in 1:N) {
   pp_kappa_fw[i] = exp(normal_rng(log_lk_kappa_fw[i] - 0.5 * sigma_y^2, sigma_y)); // generate posterior predictives with backtransform
   pp_kappa_sp[i] = exp(normal_rng(log_lk_kappa_sp[i] - 0.5 * sigma_y^2, sigma_y)); // generate posterior predictives with backtransform 
    }
    
  // for (i in 1:N) {  
  //   if(i == 1) {
  //     E[i] = fs*Ps*126004.6; // Eq 4.3 generated estimate for the amount of eggs produced that year for that stock. 
  //     }
  //   if(i>1){
  //       E[i] = fs*Ps*N_sp[i]; // Eq 4.3 generated estimate for the amount of eggs produced that year for that stock. 
  //     }
  //   N_j[i] = pp_kappa_fw[i]*E[i]; // Eq 4.4  generated estiamte for the amount of fish each year and stock that survive to a juvenile stage 
  //   N_sp[i] = pp_kappa_sp[i]*N_j[i]; // Eq 4.5 generated estiamte for the amount of fish each year and stock that survive to a spawning stage 
  // }
  
}
 
