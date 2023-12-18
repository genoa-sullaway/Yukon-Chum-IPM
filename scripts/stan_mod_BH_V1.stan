data { // all equation references are from proposal numbering
  int<lower=0> N ; // total observations 
  int<lower=0> K; // number of stocks
  // int<lower=0> ncovars; //number of covariates
  real<lower=0> Ps; // Proportion of females in spawning stock, for now just putting 50
  int<lower=0> fs; // Fecundity of each female in each stock - eventually extend for age 
  //real n_init_years[K]; // this indexes the inital spawning conditions. once htis runs will need to see how this fits with a simulation initial starting conditions 
  // stock specific data 
 // int<lower=0> N_stock[K];  // Number of data points for each stock
  real data_stage_j[N]; // vector of number of juveniles for each group
  real data_stage_sp[N];// vector of number of spawners for each group
  real <lower=0>kappa_sp_start; // adding starting values for kappa so there arent NAs..not sure if this is necessary
  real <lower=0>kappa_j_start;
 // int<lower=0> g[N];
 // int<lower = 1, upper = K> g[N]; // Vector of group assignments so STAN knows which group it is dealing with in the set of N observations 
 // real covar_1[N, ncovars]; // covariate data note: cannot be coded as matrix 
  }
  
transformed data {
// seed initial population dynamics for N_sp for each stock. 
//real N_sp_seed[N]; // predicted spawneres - this goes into the liklihood,  data involved 
real <lower=1> data_log_stage_j[N];
real <lower=1> data_log_stage_sp[N];

data_log_stage_j = log(data_stage_j); // Log transform data 
data_log_stage_sp = log(data_stage_sp);

  // for (i in 1:N) {  // seed initial population dynamics 
  //   if(i == n_init_years[1]) {
  //     N_sp_seed[i] =  uniform_rng(1,1e4); // random number generator from uniform distirbution 
  //         }
  //   if( i == n_init_years[2]) {
  //     N_sp_seed[i] = uniform_rng(1,1e4);  
  //         }
  //   if( i == n_init_years[3]) {
  //     N_sp_seed[i] = uniform_rng(1,1e4);   
  //         }
  //   }
}

parameters {
  real<lower=0> sigma_y_j; // error for each stock
  real<lower=0> sigma_y_sp; // error for each stock
  
  // Stock specific parameters
  real <lower=0> p_1;//[K]; // productivity for each stock, calculated based on parameter: average survivial, and later, covariates
  real <lower=0> p_2;//[K]; // productivity for each stock, calculated based on parameter: average survivial, and later, covariates

  real <lower=0> c_1;//[K]; // carrying capacity 
  real <lower=0> c_2;//[K]; // carrying capacity 
  
  // real <lower=1>log_N_j;
  real <lower=1>log_N_sp_start;
  real <lower=1>log_N_egg_start;
  real <lower=1>log_N_j_start;
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
real<lower=0> N_e[N]; // predicted eggs, basically a dummy step. 
real<lower=0> N_j[N]; // predicted juveniles - this goes into the liklihood, data involved 
real<lower=0> N_sp[N]; // predicted spawners - this goes into the liklihood, data involved 
real<lower=0> N_sp_start; 
real<lower=0> N_egg_start;
real<lower=0> N_j_start;
// real<lower=0> kappa_sp_start;
// real<lower=0> kappa_j_start;
 
real kappa_j[N]; // predicted survival for each stock
real kappa_sp[N]; // predicted survival for each stock

//kappa_sp_start = exp(log_kappa_sp_start);
  kappa_sp[1] = kappa_sp_start;

//kappa_j_start = exp(log_kappa_j_start);
  kappa_j[1]= kappa_j_start; 

N_sp_start = exp(log_N_sp_start); // transform predicted spawners
N_sp[1] = N_sp_start;

N_egg_start = exp(log_N_egg_start); // transform predicted eggs
N_e[1] = N_egg_start;

N_j_start = exp(log_N_j_start); // transform predicted juveniles
N_j[1] = N_j_start;

// real N_e[N]; // predicted eggs, basically a dummy step. 
// real N_j[N]; // predicted juveniles - this goes into the liklihood, data involved 
// real N_sp[N]; // predicted spawneres - this goes into the liklihood,  data involved 

  // for (i in 1:N) {  
  //   if(i == n_init_years[1]) { // if the loop starts on a new stock it will pull from the seed populatoin, if not it will keep going business as usual
  //      N_e[i] = fs*Ps*N_sp_seed[i]; // Eq 4.3 generated estimate for the amount of eggs produced that year for that stock.
  //      }
  //   if( i == n_init_years[2]) {
  //      N_e[i] = fs*Ps*N_sp_seed[i]; // Eq 4.3 generated estimate for the amount of eggs produced that year for that stock.
  //         }
  //   if( i == n_init_years[3]) {
  //      N_e[i] = fs*Ps*N_sp_seed[i]; // Eq 4.3 generated estimate for the amount of eggs produced that year for that stock.
  //         } 
  // }
  
  for (i in 2:N) { //probably need to add a loop in here for stocks too..
    N_e[i] = fs*Ps*N_sp[i-1]; // Eq 4.3 generated estimate for the amount of eggs produced that year for that stock.
    
    //kappa_j[i] =  p_1[g[i]]/ (1 + ((p_1[g[i]]*N_e[i])/c_1[g[i]])); // Eq 4.1  - Bev holt transition estimating survival from Egg to Juvenile (plugs into Eq 4.4) 
    kappa_j[i] =  p_1/ (1 + ((p_1*N_e[i])/c_1)); // Eq 4.1  - Bev holt transition estimating survival from Egg to Juvenile (plugs into Eq 4.4) 
    
    N_j[i] = kappa_j[i]*N_e[i]; // Eq 4.4  generated estiamte for the amount of fish each year and stock that survive to a juvenile stage
   
    kappa_sp[i] =  p_2/ (1 + ((p_2*N_j[i])/c_2)); // Eq 4.1   
 
   // kappa_sp[i] =  p_2[g[i]]/ (1 + ((p_2[g[i]]*N_j[i])/c_2[g[i]])); // Eq 4.1   
 
    N_sp[i] = kappa_sp[i]*N_j[i]; // Eq 4.5 generated estiamte for the amount of fish each year and stock that survive to a spawning stage
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
   sigma_y_j ~  normal(0.001, 0.1); //cauchy(0,1);
   sigma_y_sp ~  normal(0.001, 0.1); //cauchy(0,1); 
   // log_N_j ~ uniform(1,1e6); //8.0, 13.0); // log juvenile prior
   // log_N_sp ~ uniform(1,1e6); // log spawner prior 
   log_N_sp_start ~ normal(15,0.1); // 12-15
   log_N_egg_start ~ normal(18,0.1); // 12-15 
   log_N_j_start ~ normal(16,0.1); // 12-15  
   
 // kappa_j_start ~ uniform(1,0.001); //  
 // kappa_sp_start ~ uniform(1,0.001); //  

//for(k in 1:K){
  //avg_B1[k] ~ normal(10,10); // Stan specific loop to assign priors across stocks
   p_1 ~ uniform(-10,10); // uniform(0.0001,1.5); //10,10);
   p_2 ~uniform(-10,10); // uniform(0.0001,1.5);//normal(0,1.5^2); //10,10);
   
   c_1 ~ uniform(-10,10); // uniform(1,1e6);// uniform(1000,100000); // normal(1,5); <- these are my old prior values, cc are the uniform values. 
   c_2 ~ uniform(-10,10); //uniform(1,1e6);// uniform(1000,100000); // normal(1,5);
//}

// Liklilihoods -- 
  for (i in 1:N) {
    data_log_stage_j[i] ~ normal(log(N_j[i]), sigma_y_j); 
    data_log_stage_sp[i] ~ normal(log(N_sp[i]), sigma_y_sp); 
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

//generated quantities {
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
 
