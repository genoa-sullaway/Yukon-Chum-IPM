data { // all equation references are from proposal numbering
  int<lower=0> N ; // total observations 
  int<lower=0> K; // number of stocks
  // int<lower=0> ncovars; //number of covariates
  real<lower=0> Ps; // Proportion of females in spawning stock, based on lit
  int<lower=0> fs; // Fecundity of each female in each stock - eventually extend for age ?
  //real n_init_years[K]; // this indexes the inital spawning conditions. once htis runs will need to see how this fits with a simulation initial starting conditions 
  // stock specific data 
 // int<lower=0> N_stock[K];  // Number of data points for each stock
  matrix[N,K] data_stage_j; // vector of number of juveniles for each group  (basis)
  matrix[N,K] data_stage_sp;// vector of number of spawners for each group (escapement)
 
  real <lower=0>kappa_sp_start[K]; // adding starting values for kappa so there arent NAs..not sure if this is necessary
  real <lower=0>kappa_j_start[K];
  
  // related to covariates: 
  matrix[N,K] cov1;
  matrix[N,K] cov2;
  
  real <lower=0> basal_p_1[K]; // mean survival absent of density dependence - for now just add it in dont estimate it. 
  real <lower=0> basal_p_2[K];
 // int<lower=0> g[N];
 // int<lower = 1, upper = K> g[N]; // Vector of group assignments so STAN knows which group it is dealing with in the set of N observations 
 // real covar_1[N, ncovars]; // covariate data note: cannot be coded as matrix 
  }
  
transformed data {
// seed initial population dynamics for N_sp for each stock. 
//real N_sp_seed[N]; // predicted spawneres - this goes into the liklihood,  data involved 
  matrix[N,K] data_log_stage_j;
  matrix[N,K] data_log_stage_sp;

data_log_stage_j = log(data_stage_j); // Log transform data 
data_log_stage_sp = log(data_stage_sp);
}

parameters {
    real<lower=0>sigma_y_j[K];
    real<lower=0>sigma_y_sp[K];
    
    // matrix[N,K] sigma_y_j;
    // matrix[N,K] sigma_y_sp;
  // real<lower=0> sigma_y_j; // error for juveniles
  // real<lower=0> sigma_y_sp; // error for spawners
  
  // Stock specific parameters
  // real <lower=0> p_1;//[K]; // productivity for each stock, calculated based on parameter: average survivial, and later, covariates
  //real <lower=0> p_2;//[K]; // productivity for each stock, calculated based on parameter: average survivial, and later, covariates

  real<lower=0>c_1;//[K]; //[K]; // carrying capacity 
  real<lower=0>c_2[K]; //[K]; // carrying capacity 
  
  // covariate parameters 
  real theta1[K];
  real theta2[K];
  
  // real<lower=0> N_sp[N]; // spawners
  // real <lower=1>log_N_j;
  real log_N_sp_start[K];
  real log_N_egg_start[K];
  real log_N_j_start[K];
 
  // real <lower=0>kappa_sp_start;
  // real <lower=0>kappa_j_start;
}
  // real avg_B1[K]; // mean survival rate used to estimate p actual survivial rate, later coefficients will be added in here
  // real avg_B2[K]; // mean survival rate used to estimate p actual survivial rate, later coefficients will be added in here
  
  // real<lower=0> stage_j_pred[N]; // juvenile predicted incorporates spawn data from the likelihood [MF included spawner error too]
  // real<lower=0> stage_sp_pred[N]; // spawner predicted incorporates spawn  data from the likelihood 

  //COEFFICIENT
 // real theta[K, ncovars]; // covariate estimated for each covariate and each population 
 // real mu_coef[ncovars]; // mean covariate effect across populations (group level hierarchical effect)
 // real<lower=0> sigma_coef[ncovars]; // error of the covariate effect across populations (group level hierarchical effect)

transformed parameters { 
matrix[N,K] N_e; // predicted eggs, basically a dummy step. 
matrix[N,K] N_j; // predicted juveniles - this goes into the liklihood, data involved 
matrix[N,K] N_sp; // predicted spawners - this goes into the liklihood, data involved 

real<lower=0> N_sp_start[K]; 
real<lower=0> N_egg_start[K];
real<lower=0> N_j_start[K];
matrix[N,K] p_1;
matrix[N,K] p_2;
// 
// real <lower=0> c_1;//[K]; // carrying capacity 
// real <lower=0> c_2;//[K]; // carrying capacity 
  
// real<lower=0> kappa_sp_start;
// real<lower=0> kappa_j_start;
 
real kappa_j[N,K]; // predicted survival for each stock
real kappa_sp[N,K]; // predicted survival for each stock

// c_1 = exp(log_c_1);//[K]; // carrying capacity 
// c_2 = exp(log_c_2);//[K]; // carrying capacity 
//   

  for (k in 1:K) {
  kappa_sp[1,k] = kappa_sp_start[k];
//kappa_j_start = exp(log_kappa_j_start);
  kappa_j[1,k]= kappa_j_start[k]; 
 
  N_sp_start[k] = exp(log_N_sp_start[k]); // transform predicted spawners
  N_egg_start[k] = exp(log_N_egg_start[k]); // transform predicted eggs
  N_j_start[k] = exp(log_N_j_start[k]); // transform predicted juveniles

  N_sp[1,k] = N_sp_start[k];
  N_e[1,k] = N_egg_start[k];
  N_j[1,k] = N_j_start[k];
    
    }
  for (k in 1:K){
    for (i in 1:N) {
  p_1[i,k]  = 1 / exp(-basal_p_1[k] - (theta1[k]*cov1[i,k]));
  p_2[i,k]  = 1 / exp(-basal_p_2[k] - (theta2[k]*cov2[i,k]));
     }
}
  
for(k in 1:K){  // loop for each population
  for (i in 2:N) { //will need to add a loop in here for stocks too..
    N_e[i,k] = fs*Ps*N_sp[i-1,k]; // Eq 4.3 generated estimate for the amount of eggs produced that year for that stock.
    
    kappa_j[i,k] =  p_1[i,k]/ (1 + ((p_1[i,k]*N_e[i,k])/c_1));// [k])); // Eq 4.1  - Bev holt transition estimating survival from Egg to Juvenile (plugs into Eq 4.4) 
    
    N_j[i,k] = kappa_j[i,k]*N_e[i,k]; // Eq 4.4  generated estiamte for the amount of fish each year and stock that survive to a juvenile stage
   
    kappa_sp[i,k] =  p_2[i,k]/ (1 + ((p_2[i,k]*N_j[i,k])/c_2[k])); // Eq 4.1   - Bev holt transition estimating survival from juvenile to spawner (plugs into Eq 4.4) 
   
    N_sp[i,k] = kappa_sp[i,k]*N_j[i,k]; // Eq 4.5 generated estiamte for the amount of fish each year and stock that survive to a spawning stage
   }
}

}

	// 		 for(y in 1:N){
	// 		 	for(c in 1:ncovars){
	// 		  cov_eff[y,c]= theta[g[y],c]*covars[y, c]; // g[y] is the population pointer vector so theta is estimated for each population for each covariate, not every year. See ragged and missing data structure in stan manual for structure
	//  } // next c
	// } //next y
 
   // p_1[K] = 1 + exp(avg_B + sum(cov_eff[y,1:ncovars])) // Eq 4.2 FW juvenile stage - productivity funtion of average survival rate and sum of environmental covariates
   // p_2[K] = 1 + exp(avg_B + sum(cov_eff[y,1:ncovars])) // Eq 4.2 ocean stage- productivity funtion of average survival rate and sum of environmental covariates

model {
  // PRIORS
   // sigma_y_j ~  normal(2,5);//uniform(0,2000);// normal(0.001, 0.1); //cauchy(0,1);
   // sigma_y_sp ~ normal(2,5);// uniform(0,2000);// normal(0.001, 0.1); //cauchy(0,1); 
   
   // log_N_j ~ uniform(1,1e6); //8.0, 13.0); // log juvenile prior
   // log_N_sp ~ uniform(1,1e6); // log spawner prior 
    
   //    for(i in 1:N) {
   // N_sp[i] ~ normal(data_stage_sp[i], sigma_y_sp); //  
   //    }   
   
     for(k in 1:K){
   sigma_y_j[k] ~  normal(2,5);//uniform(0,2000);// normal(0.001, 0.1); //cauchy(0,1);
   sigma_y_sp[k] ~ normal(2,5);// uniform(0,2000);// normal(0.001, 0.1); //cauchy(0,1); 
   
   log_N_egg_start[k] ~ normal(21,10); //  
   log_N_j_start[k] ~ normal(16,10); //  
   log_N_sp_start[k] ~ normal(14,10); //  
   
   theta1[k]~normal(0.1,5);
   theta2[k]~normal(-0.2,10);

   // c_1[k] ~ normal(1e7,1e7); //16.1,5);//uniform(0, 20000000); // uniform(1,1e6);// uniform(1000,100000); // normal(1,5); <- these are my old prior values, cc are the uniform values.
   // c_2[k] ~ normal(750000,1e7); //13.8,3);
     } 
     
     c_1 ~ normal(1e7, 1e8);
    //c_1[1] ~ normal(1e7, 1e7);
    // c_1[2] ~ normal(2.5e7, 1e7);
    // c_1[3] ~ normal(1.77e7, 1e7);
        
    c_2[1] ~ normal(750000, 1e7); 
    c_2[2] ~ normal(250000, 1e7);
    c_2[3] ~ normal(177000, 1e7);
             
// Covariate data ===================
     
 // kappa_j_start ~ uniform(1,0.001); //  
 // kappa_sp_start ~ uniform(1,0.001); //  

//for(k in 1:K){
  //avg_B1[k] ~ normal(10,10); // Stan specific loop to assign priors across stocks
   // p_1 ~ normal(0.05,0.01); // uniform(0.0001,1.5); //10,10);
  // p_2 ~ normal(0.15,0.01); // uniform(0.0001,1.5);//normal(0,1.5^2); //10,10);
  // c_1 ~ uniform(10e5, 10e9); // uniform(1,1e6);// uniform(1000,100000); // normal(1,5); <- these are my old prior values, cc are the uniform values.
  // c_2 ~ uniform(10e3,10e6);
  // 
  // c_1 ~ normal(1e7,1e7);//16.1,5);//uniform(0, 20000000); // uniform(1,1e6);// uniform(1000,100000); // normal(1,5); <- these are my old prior values, cc are the uniform values.
  // c_2 ~ normal(750000,1e7);//13.8,3);
  // 
  // log_c_1 ~ lognormal(16.1,5);//uniform(0, 20000000); // uniform(1,1e6);// uniform(1000,100000); // normal(1,5); <- these are my old prior values, cc are the uniform values.
  // log_c_2 ~ lognormal(13.8,3);//uniform(0, 2000000); //uniform(1,1e6);// uniform(1000,100000); // normal(1,5);
//}

// target += normal_lpdf(log(N_e)|N_e,20);

// Liklilihoods -- 
for(k in 1:K){
  for (i in 1:N) {
    data_log_stage_j[i,k] ~ normal(log(N_j[i,k]), sigma_y_j[k]);
    data_log_stage_sp[i,k] ~ normal(log(N_sp[i,k]), sigma_y_sp[k]);
  } 
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
   
  real pp_log_N_j[N,K]; // predicted recruits 
  real pp_log_N_sp[N,K]; // predicted spawners 
  
 for (k in 1:K){
   for (i in 1:N) {
   pp_log_N_j[i,k] = (normal_rng(log(N_j[i,k]) - 0.5 * sigma_y_j[k]^2, sigma_y_j[k])); // generate posterior predictives with backtransform? 
   pp_log_N_sp[i,k] = (normal_rng(log(N_sp[i,k]) - 0.5 * sigma_y_sp[k]^2, sigma_y_sp[k])); // generate posterior predictives with backtransform? 
    }
 }
}

// 	real<lower=0> pp_kappa_j[N]; // predicted survival stage fw to juvenile
// 	real<lower=0> pp_kappa_sp[N]; // predicted survival stage spawner
// 	// real<lower=0> E[N]; // Eggs	
// 	// real<lower=0> N_j[N];	// Number of fish surviving to juvenile
// 	// real<lower=0> N_sp[N];	// number of fish surviving to spawn
// 
//   //   real mu_coef_rep[ncovars];
//   //   // use current estimate of mu_coef to generate new sample
//   //   for (c in 1:ncovars) {
//   //     mu_coef_rep[c] = normal_rng(mu_coef[c],sigma_coef[c]);
//   //    }
//  
//   for (i in 1:N) {
//     pp_kappa_j[i] = normal_rng(log(kappa_j[i]), sigma_y_j); // generate posterior predictives - currently coded like I am not estimating on log scale. will need to check this. 
//     pp_kappa_sp[i] = normal_rng(log(kappa_sp[i]), sigma_y_sp);
//     
//     // if I estimate on the log scale!! 
//    // pp_kappa_j[i] = exp(normal_rng(kappa_j[i] - 0.5 * sigma_y^2, sigma_y)); // generate posterior predictives with backtransform
//    // pp_kappa_sp[i] = exp(normal_rng(kappa_sp[i] - 0.5 * sigma_y^2, sigma_y)); // generate posterior predictives with backtransform 
//     }
//}
 
