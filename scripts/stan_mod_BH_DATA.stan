data { // all equation references are from proposal numbering
  int nByrs ; // number of brood years  
  int nRyrs;  // Number of recruitment years in SR model
  int A;     // Number of (total) age classes
  int<lower=0> K; // number of stocks
  real<lower=0> Ps; // Proportion of females in spawning stock, based on lit
  int<lower=0> fs[A]; // Fecundity of each female in each stock - eventually extend for age ?
  
  matrix[nByrs,K] data_stage_j;    // vector of number of juveniles for each group  (basis)
  matrix[nRyrs,K] data_stage_harvest;   // vector of number of spawners for each group (harvest)
  matrix[nRyrs,K] data_stage_sp;   // vector of number of spawners for each group (escapement)
  matrix[1,K] sigma_y_j;   // Initially fix sigma 
  matrix[1,K] sigma_y_sp;  // Initially fix sigma 
  
  matrix[1,K] kappa_marine_start; // adding starting values for kappa so there arent NAs..not sure if this is necessary
  matrix[1,K] kappa_j_start;
  
  int<lower=0> ncovars1; //number of covariates for first lifestage
  int<lower=0> ncovars2; //number of covariates for second lifestage 

  real cov1[nByrs, ncovars1]; // covariate data in a matrix format 
  real cov2[nByrs, ncovars2]; 
  
// this should be chagned back when I use more stocks...
  real basal_p_1; // mean survival absent of density dependence - for now just add it in dont estimate it. 
  real basal_p_2;
  // real <lower=0> basal_p_1[K]; // mean survival absent of density dependence - for now just add it in dont estimate it. 
  // real <lower=0> basal_p_2[K];

  // age comps 
  vector<lower=0,upper=1>[4] prob;  // Maturity schedule probs - currently data, eventually will probably be random effect, 4 long because of 4 age classes
  //matrix<lower=0>[nRyrs, K,A] o_run;      // Observed run size by age class
  real o_run[nRyrs, K,A]; // Observed run size by age class
  real<lower=0, upper=1> o_run_comp[nRyrs, K,A]; // Observed age composition by year
  vector [nRyrs] ess_age_comp;   // Effective input sample size for age comp "observations"
}
  
transformed data {
// seed initial population dynamics 
  matrix[nByrs,K] data_log_stage_j;
  matrix[nRyrs,K] data_log_stage_sp;
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
  
  real log_N_sp_start[K,A];
  real log_N_egg_start[K,A];
  real log_N_j_start[K];
  real log_N_recruit_start[K];
  real log_N_e_sum_start[K];
  
  real<lower=0,upper=1> D_scale;     // Variability of age proportion vectors across cohorts
  matrix<lower=0.01> [nRyrs, A] g;    // Individual year/age class gamma variates for generating age at maturity proportions
}

transformed parameters { 
matrix[nByrs,K] N_j; // predicted juveniles - this goes into the liklihood, data involved 
matrix[nByrs,K] N_recruit;  // predicted recruits - before they get assigned to returning age classes
  
real <lower=0> H_b[nByrs,K,A]; // harvest at river mounth by age and stock, gets applied to recruits to get number of spawners  
  
// variables with age class 
real<lower=0> N_sp[nByrs,K,A];
real<lower=0> N_returning[nByrs,K,A];
real<lower=0> N_e[nByrs,K,A];
matrix[nByrs,K] N_e_sum; // sum eggs across ages to then go into the lifecycle section that doesnt use age 

real N_sp_start[K,A];
real N_egg_start[K,A];
real N_j_start[K]; 
real N_recruit_start[K];
real N_e_sum_start[K];

// survival and covariate section 
matrix[nByrs,K] p_1; // productivity in bev holt transition funciton,1 = FW early marine 
matrix[nByrs,K] p_2; // productivity in bev holt transition funciton, 2 = later marine 

real kappa_j[nByrs,K]; // predicted survival for each stock
real kappa_marine[nByrs,K]; // predicted survival for each stock
 
real cov_eff1[nByrs, K, ncovars1]; // array that holds FW and early marine covariate effects by brood year and stock
real cov_eff2[nByrs, K, ncovars2];// array that holds marine covariate effects by brood year and stock
 
real i; // index attempt t-A+a 
 
// Age related transformed params ====== 
matrix<lower=0, upper=1>[nByrs, A] p;  // Age at maturity proportions
vector<lower=0, upper=1>[4] pi;        // Maturity schedule probs
real<lower=0> D_sum;                   // Inverse of D_scale which governs variability of age proportion vectors across cohorts
vector<lower=0>[A] Dir_alpha;          // Dirichlet shape parameter for gamma distribution used to generate vector of age-at-maturity proportions
real<lower=0, upper=1> q[nRyrs,K, A];   // Age composition by year/age classr matrix

// starting value transformations ======
 for (k in 1:K) {
  kappa_marine[1,k] = kappa_marine_start[1,k]; 
  kappa_j[1,k]= kappa_j_start[1,k]; 
 
 //transform startign values 
  N_sp_start[k,1:A] = exp(log_N_sp_start[k,1:A]); // transform predicted spawners
  N_egg_start[k,1:A] = exp(log_N_egg_start[k,1:A]); // transform predicted eggs 
  N_j_start[k] = exp(log_N_j_start[k]); // transform predicted juveniles
  N_recruit_start[k] = exp(log_N_recruit_start[k]); // transform predicted recruits
  N_e_sum_start[k] = exp(log_N_e_sum_start[k]); // transform eggs summed across ages 
  
  // add starting values to the whole population array 
  N_sp[1,k,1:A] = N_sp_start[k,1:A];
  N_e[1,k,1:A] = N_egg_start[k,1:A]; 
  // no age index on population stage
  N_j[1,k] = N_j_start[k]; 
  N_recruit[1,k] = N_recruit_start[k]; 
    }
   
// the cov effects need seperate loop because number of covariates varies between lifestage
   for (k in 1:K){
   for(t in 1:nByrs){
   for (c in 1:ncovars1) {
  cov_eff1[t,k,c] = theta1[k,c]*cov1[t,c];
   }
  }
}

for (k in 1:K){
    for(t in 1:nByrs){
     for (c in 1:ncovars2) {
  cov_eff2[t,k,c] = theta2[k,c]*cov2[t,c];
   }
  }
}
   // calculate productivity based on covariates for each lifestage 
 for(c in 1:ncovars1){
  for (k in 1:K){
    for (t in 1:nByrs) {
     p_1[t,k]  = 1 / exp(-basal_p_1-sum(cov_eff1[t,k,1:c]));// sum(theta1[k,]*cov1[i,])); # estimate theta for each popualtion and each covariate, not every year 
   }
  }
 }
 
for(c in 1:ncovars2){
  for (k in 1:K){
    for (t in 1:nByrs) { 
     p_2[t,k]  = 1 / exp(-basal_p_2 -sum(cov_eff2[t,k,1:c]));
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
    for (t in 1:nRyrs) {
      p[t,a] = g[t,a]/sum(g[t,]);
    }
  }

  // Calculate the numbers at age matrix as brood year recruits at age (proportion that matured that year)
  //from CC and I dont need it, the same code is below, but keeping it to remember process 
  // for (t in 1:nRyrs) {
  //   for(a in 1:A){
  //     N_returning[t,a] = R[t+A-a] * p[t+A-a,a];
  //   }
  // }
  
for(k in 1:K){  // loop for each population
  for (t in 2:nByrs){ //this should maube be 1??
   
    kappa_j[t,k] =  p_1[t,k]/ (1 + ((p_1[t,k]*N_e_sum[t,k])/c_1[k])); // Eq 4.1  - Bev holt transition estimating survival from Egg to Juvenile (plugs into Eq 4.4) 
    
    N_j[t,k] = kappa_j[t,k]*N_e_sum[t,k]; // Eq 4.4  generated estiamte for the amount of fish each year and stock that survive to a juvenile stage
   
    kappa_marine[t,k] =  p_2[t,k]/ (1 + ((p_2[t,k]*N_j[t,k])/c_2[k])); // Eq 4.1   - Bev holt transition estimating survival from juvenile to spawner (plugs into Eq 4.4) 
   
    N_recruit[t,k] = kappa_marine[t,k]*N_j[t,k]; // Eq 4.5 generated estiamte for the amount of fish each year and stock that survive to a spawning stage
 
  //add in age for stage where I am tracking age class.... 
    //for (c in 1:nRyrs) { // swtich to calendar years using t+A-a
      for (a in 1:A) { 
    N_returning[t+A-a,k,a] = N_recruit[t,k]*p[t+A-a,a];  // currys indexing here: N_ta[t,a] = R[t+A-a] * p[t+A-a,a];
    
    N_sp[t+A-a,k,a] = N_returning[t+A-a,k,a] - H_b[t+A-a,k,a];    
    
    N_e[t+A-a,k,a] = fs[a]*Ps*N_sp[t+A-a,k,a]; // Eq 4.3 generated estimate for the amount of eggs produced that year for that stock.
  
      // transition back to brood years 
    N_e_sum[t,k] = (N_e[t+A-a,k,1]+N_e[t+A-a,k,2]+N_e[t+A-a,k,3]+N_e[t+A-a,k,4]);
    
    // }
    }
    
    // add in a step where eggs are summed across ages //
   //  N_e_sum[t,k] = (N_e[t,k,1]+N_e[t,k,2]+N_e[t,k,3]+N_e[t,k,4]);
    
   }
 }
// I dont know if this needs to be looped or not....

// SAVING ORIGINAL LOOP JUST INCASE ......
// for(k in 1:K){  // loop for each population
//   for (t in 2:nByrs){ 
//    
//     kappa_j[t,k] =  p_1[t,k]/ (1 + ((p_1[t,k]*N_e_sum[t,k])/c_1[k])); // Eq 4.1  - Bev holt transition estimating survival from Egg to Juvenile (plugs into Eq 4.4) 
//     
//     N_j[t,k] = kappa_j[t,k]*N_e_sum[t,k]; // Eq 4.4  generated estiamte for the amount of fish each year and stock that survive to a juvenile stage
//    
//     kappa_marine[t,k] =  p_2[t,k]/ (1 + ((p_2[t,k]*N_j[t,k])/c_2[k])); // Eq 4.1   - Bev holt transition estimating survival from juvenile to spawner (plugs into Eq 4.4) 
//    
//     N_recruit[t,k] = kappa_marine[t,k]*N_j[t,k]; // Eq 4.5 generated estiamte for the amount of fish each year and stock that survive to a spawning stage
//  
//   //add in age for stage where I am tracking age class.... 
//   for (a in 1:A) {
//    // i=t+A-a;
//     N_returning[t,k,a] = N_recruit[t,k]*p[t+A-a,a];  // currys indexing here: N_ta[t,a] = R[t+A-a] * p[t+A-a,a];
//     
//     N_sp[t,k,a] = N_returning[t,k,a] - H_b[t,k,a];    
//     
//     N_e[t,k,a] = fs[a]*Ps*N_sp[t,k,a]; // Eq 4.3 generated estimate for the amount of eggs produced that year for that stock.
//       // N_sp ^^ needs to be translated between brood and recuirt/calendar years somehow.... 
//     }
//     
//     // add in a step where eggs are summed across ages //
//     N_e_sum[t,k] = (N_e[t,k,1]+N_e[t,k,2]+N_e[t,k,3]+N_e[t,k,4]);
//     
//    }
//  }
 
  // Calculate age proportions by return year
  for(k in 1:K){
   for (t in 1:nRyrs) {
    for(a in 1:A){
      q[t,k,a] = N_returning[t,k,a]/sum(N_returning[t,k,1:A]);
    }
  }
}

}// close block

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

 // SRA likelihood
  // Gamma variates for each year and age class which are used to determine age at maturity proportions
  for (y in 1:nRyrs) {
    for (a in 1:A) {
      //g[y,a] ~ gamma(Dir_alpha[a],1);
      target += gamma_lpdf(g[y,a]|Dir_alpha[a],1);
    }
  }

  // First `a.max` years of recruits, for which there is no spawner link
  //lnR[1:a_max] ~ normal(mean_ln_R0, sigma_R0);

  // State model -- no state space right now so currenlty not including this part 
  // lnR[(A+a_min):nRyrs] ~ normal(lnRm_2[(A+a_min):nRyrs], sigma_R);



  // Observation model
  for(k in 1:K){ // stocks 
  for(t in 1:nRyrs){ // calendar years 
  // if ESS isnt a random variable, I dont think I need this section. Currently ESS is fixed. 
    // if(sum(data_log_stage_sp[t,1:K])>0){
   //  target += (ess_age_comp[t]*o_run_comp[t,k,1:A])*sum(  .* log(q[t,k,1:A])); // time varying ESS for age comp likelihood for ONLY years with data
    // OG code not touching -- // target += ess_age_comp[t]*sum(o_run_comp[t,k,1:A] .* log(q[t,k,1:A])); // time varying ESS for age comp likelihood for ONLY years with data
     // }
    target += normal_lpdf(log(data_log_stage_sp[t,k]) | log(sum(N_sp[t,k,1:A])), sigma_y_sp[1,k]);
    target += normal_lpdf(log(data_stage_harvest[t,k]) | log(sum(H_b[t,k,1:A])), sigma_y_sp[1,k]);
       }
  }

for(k in 1:K){
  for (t in 2:nByrs) {
    data_log_stage_j[t,k] ~ normal(log(N_j[t,k]), sigma_y_j[1,k]);
    } 
  }
}  

generated quantities {
  real pp_log_N_j[nByrs,K]; // predicted recruits
  real pp_log_N_sp[nRyrs,K,A]; // predicted spawners
  //real pp_log_N_r[N-1,K];

 for (k in 1:K){
   
   for (t in 1:nByrs) {
   pp_log_N_j[t,k] = (normal_rng(log(N_j[t,k]) - 0.5 * sigma_y_j[1,k]^2, sigma_y_j[1,k])); // generate posterior predictives with backtransform?
    }
   for (t in 1:nRyrs) {
     for(a in 1:A){
   pp_log_N_sp[t,k,a] = (normal_rng(log(N_sp[t,k,a]) - 0.5 * sigma_y_sp[1,k]^2, sigma_y_sp[1,k])); // generate posterior predictives with backtransform?
    }
   }
 }
 
}

