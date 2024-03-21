data { // all equation references are from proposal numbering
  int<lower=0> nByrs ; // number of brood years  
  int<lower=0> nRyrs;  // Number of recruitment years in SR model
  int<lower=0> A;     // Number of age classes - 4
  int<lower=0> K; // number of stocks - currently set to 1 for initial model test runs 
  real<lower=0> Ps; // Proportion of females in spawning stock, based on lit - currently 50%
  real fs[A,K]; // Fecundity of each female in each stock - eventually extend for age ?
   
  matrix[nByrs,K] data_stage_j;    // number of juveniles for each group  (basis)
  matrix[nRyrs,K] data_stage_return;   //  number of harvest + escapement for each group 
  matrix[nRyrs,K] data_stage_sp;   // number of spawners for each group (escapement)
  matrix[1,K] sigma_y_j;  // Initially fix sigma - process error for juveniles
  matrix[1,K] sigma_y_r;  // Initially fix sigma - process error for returns
  matrix[1,K] sigma_y_sp; // Initially fix sigma - process error for spawners
 
  real <lower=0> H_b[nRyrs,K,A]; // harvest at river mounth by age and stock, treating it as data, multipling harvest by known age comp. gets applied to recruits to get number of spawners. 
   
   // starting values for popualtion stages --feeding in as data
  real log_N_sp_start[A,K,A]; // fill first 4 years with starting values so there arent NAs due to brood year/cal year transformation
  real log_N_returning_start[A,K,A]; // fill first 4 years with starting values so there arent NAs due to brood year/cal year transformation
  real log_N_egg_start[A,K,A];
  real log_N_j_start[K,1];
  real log_N_recruit_start[K,1];
  real log_N_e_sum_start[K,1];
  
  // real c_1[K,1]; //fixing carrying capacity to test model
  // real c_2[K,1]; //fixing carrying capacity to test model
  
// kappa is marine and juvenile survival estimated via beverton holt transition fxn 
  matrix[1,K] kappa_marine_start; // adding starting values for kappa so there arent NAs..not sure if this is necessary
  matrix[1,K] kappa_j_start;
  
  // int<lower=0> ncovars1; //number of covariates for first lifestage - starting with 1
  // int<lower=0> ncovars2; //number of covariates for second lifestage - starting with 1
// 
//   real sigma_coef1[K,ncovars1]; // initially fix, error of the covariate effect across populations 
//   real sigma_coef2[K,ncovars2]; // initially fix, error of the covariate effect across populations 

  // real cov1[nByrs, ncovars1]; // covariate data in a matrix format 
  // real cov2[nByrs, ncovars2]; 
  // 
  // real basal_p_1; // mean survival absent of density dependence - for now just add it in dont estimate it. 
  // real basal_p_2;
  
  // real <lower=0> basal_p_1[K]; // mean survival absent of density dependence - for now just add it in dont estimate it. 
  // real <lower=0> basal_p_2[K];

  // age comps 
  vector<lower=0,upper=1>[4] prob;  // Maturity schedule probs - currently fixed, eventually will test as a random effect, 4 long because of 4 age classes
  
  // matrix<lower=0>[nRyrs, K,A] o_run;      // Observed run size by age class
  // real o_run[nRyrs, K,A]; // Observed run size by age class
  // real<lower=0, upper=1> o_run_comp[nRyrs, K,A]; // Observed age composition by year
  // vector [nRyrs] ess_age_comp;   // Effective input sample size for age comp "observations" -  currently fixed to 200 based on Hulson et al 2011
}
  
transformed data {
// seed initial population dynamics 
//   matrix[nByrs,K] data_log_stage_j;
//   matrix[nRyrs,K] data_log_stage_sp;
//  // matrix[N,K] data_log_stage_r;
// 
// //data_log_stage_r = log(data_stage_r);
// data_log_stage_j = log(data_stage_j); // Log transform data 
// data_log_stage_sp = log(data_stage_sp);

}

parameters {
// real<lower=0>sigma_y_j[K];
//  real<lower=0>sigma_y_sp[K];

  real log_c_1[K,1]; // log carrying capacity
  real log_c_2[K,1]; // log carrying capacity
  // 
  // covariate parameters 
  // real theta1[K, ncovars1]; // covariate estimated for each covariate and each population 
  // real theta2[K,ncovars2];
  
 real log_p_1[K,1]; // covariate estimated for each covariate and each population 
 real log_p_2[K,1];
  
  real<lower=0,upper=1> D_scale;     // Variability of age proportion vectors across cohorts
  matrix<lower=0.01> [nRyrs, A] g;    // Individual year/age class gamma variates for generating age at maturity proportions
  real log_catch_q[K,1];
  // vector<lower=0.0, upper=4.0>[nRyrs] log_fm; // instantaneous fishing mortality parameter not used right now

  
  // real mu_coef1[ncovars1]; // mean covariate effect across populations (group level hierarchical effect)
  // real mu_coef2[ncovars2];
  // 
  // real sigma_coef1[ncovars1]; // error of the covariate effect across populations (group level hierarchical effect)
  // real sigma_coef2[ncovars2]; 
  
  // starting values for popualtion stages 
  // real log_N_sp_start[A,K,A]; // fill first 4 years with starting values so there arent NAs due to brood year/cal year transformation
  // real log_N_returning_start[A,K,A]; // fill first 4 years with starting values so there arent NAs due to brood year/cal year transformation
  // real log_N_egg_start[A,K,A];
  // real log_N_j_start[K];
  // real log_N_recruit_start[K]; 
  // real log_N_e_sum_start[K];

}

transformed parameters { 
matrix[nByrs,K] N_j; // predicted juveniles -calculated
matrix[nByrs,K] N_j_predicted; // predicted juveniles this goes into the liklihood- gets transformed by estimates of Q
matrix[nByrs,K] N_recruit;  // predicted recruits - before they get assigned to returning age classes
matrix[nByrs,K] N_e_sum; // sum eggs across ages to then go into the lifecycle section that doesnt use age 
                       
// variables where we index with age class 
real N_sp[nRyrs,K,A];
real N_returning[nRyrs,K,A];
real N_e[nRyrs,K,A];

// starting values 
real N_sp_start[A,K,A];
real N_returning_start[A,K,A]; // this needs to be an array because best to fill in age structure of first 4 lines
real N_egg_start[A,K,A];
real N_j_start[K]; 
real N_recruit_start[K];
real N_e_sum_start[K];

// survival and covariate section 
// matrix[nByrs,K] p_1; // productivity in bev holt transition funciton, 1 = FW early marine 
// matrix[nByrs,K] p_2; // productivity in bev holt transition funciton, 2 = later marine 

real kappa_j[nByrs,K]; // predicted survival for juvenile fish (FW and early marine)
real kappa_marine[nByrs,K]; // predicted survival for marine fish
 
// real cov_eff1[nByrs, K, ncovars1]; // array that holds FW and early marine covariate effects by brood year and stock
// real cov_eff2[nByrs, K, ncovars2];// array that holds marine covariate effects by brood year and stock
real catch_q[K,1]; // related juvebile data to spawner data (on different scales) gets transfomed from log to number 

real c_1[K]; // estimate on log, transform back to normal scale 
real c_2[K]; // estimate on log, transform back to normal scale 
  
real p_1[K]; // estimate on log, transform back to normal scale 
real p_2[K]; // estimate on log, transform back to normal scale 
  
// Age related transformed params ====== 

matrix<lower=0, upper=1>[nRyrs, A] p;  // Age at maturity proportions
vector<lower=0, upper=1>[4] pi;        // Maturity schedule probabilities
real<lower=0> D_sum;                   // Inverse of D_scale which governs variability of age proportion vectors across cohorts
vector<lower=0>[A] Dir_alpha;          // Dirichlet shape parameter for gamma distribution used to generate vector of age-at-maturity proportions
real<lower=0, upper=1> q[nByrs,K, A]; 

// starting value transformations ======
 for (k in 1:K) {
  kappa_marine[1,k] = kappa_marine_start[1,k]; 
  kappa_j[1,k]= kappa_j_start[1,k]; 
 
 //transform starting values 
  N_sp_start[1:A,k,1:A] = exp(log_N_sp_start[1:A,k,1:A]); // transform predicted spawners
  N_returning_start[1:A,k,1:A] = exp(log_N_returning_start[1:A,k,1:A]); // transform predicted recruits
  N_egg_start[1:A,k,1:A] = exp(log_N_egg_start[1:A,k,1:A]); // transform predicted eggs 
  N_j_start[k] = exp(log_N_j_start[k,1]); // transform predicted juveniles
  N_recruit_start[k] = exp(log_N_recruit_start[k,1]); // transform predicted recruits
  N_e_sum_start[k] = exp(log_N_e_sum_start[k,1]); // transform eggs summed across ages 
  
  // add starting values to the whole population array 
  N_sp[1:A,k,1:A] = N_sp_start[1:A,k,1:A];
  N_e[1:A,k,1:A] = N_egg_start[1:A,k,1:A]; 
  N_returning[1:A,k, 1:A] = N_returning_start[1:A,k, 1:A]; 
  N_e_sum[1,k] = N_e_sum_start[k];
  
  // no age index on population stage
  N_j[1,k] = N_j_start[k]; 
  N_recruit[1,k] = N_recruit_start[k]; 
    }
   
  // transform log carrying capacity to normal scale
   for(k in 1:K){
   c_1[k] = exp(log_c_1[k,1]);
   c_2[k] = exp(log_c_2[k,1]);
   
   p_1[k] = exp(log_p_1[k,1]);
   p_2[k] = exp(log_p_2[k,1]);
  }

 // the cov effects need seperate loop because number of covariates varies between lifestage (currently both 1 - eventually will vary)
//    for (k in 1:K){
//    for(t in 1:nByrs){
//    for (c in 1:ncovars1) {
//   cov_eff1[t,k,c] = theta1[k,c]*cov1[t,c];
//    }
//   }
// }
// 

// for (k in 1:K){
//     for(t in 1:nByrs){
//      for (c in 1:ncovars2) {
//   cov_eff2[t,k,c] = theta2[k,c]*cov2[t,c];
//    }
//   }
// }

   // calculate productivity based on covariates for each lifestage 
//  for(c in 1:ncovars1){
//   for (k in 1:K){
//     for (t in 1:nByrs) { // this will need to be updated when adding more stocks and covariates to what is behind the slashes below 
//      p_1[t,k]  = 1 / (1 + exp(-basal_p_1- sum(cov_eff1[t,k,1:c])));  
//    }
//   }
//  }
//  
// for(c in 1:ncovars2){
//   for (k in 1:K){
//     for (t in 1:nByrs) { 
//      p_2[t,k]  = 1 / (1 + exp(-basal_p_2 -cov_eff2[t,k,c])); 
//    }
//   }
//  }

 // Maturity schedule: use a common maturation schedule to draw the brood year specific schedules
 // currently fixing this by supplying prob in data section, but will likely eventually have it as a random variable.  
  pi[1] = prob[1];  
  pi[2] = prob[2] * (1 - pi[1]);
  pi[3] = prob[3] * (1 - pi[1] - pi[2]);
  pi[4] = 1 - pi[1] - pi[2] - pi[3];
  D_sum = 1/D_scale^2;

  for (a in 1:A) {
    Dir_alpha[a] = D_sum * pi[a];
  }
    for (a in 1:A) {
      for (t in 1:nRyrs) {
      p[t,a] = g[t,a]/sum(g[t,]);
     }
    }

catch_q[K,1] = exp(log_catch_q[K,1]); // Q to relate basis data to recruit/escapement data -- Is this right??
 
for(k in 1:K){  // loop for each population
  for (t in 2:nByrs){  
   
    kappa_j[t,k] =  p_1[k]/ (1 + ((p_1[k]*N_e_sum[t-1,k])/c_1[k])); // Eq 4.1  - Bev holt transition estimating survival from Egg to Juvenile (plugs into Eq 4.4) 
    
    //kappa_j[t,k] =  p_1[t,k]/ (1 + ((p_1[t,k]*N_e_sum[t-1,k])/c_1[k])); // Eq 4.1  - Bev holt transition estimating survival from Egg to Juvenile (plugs into Eq 4.4) 
    
    N_j[t,k] = kappa_j[t,k]*N_e_sum[t-1,k]; // Eq 4.4  generated estiamte for the amount of fish each year and stock that survive to a juvenile stage
   
    kappa_marine[t,k] =  p_2[k]/ (1 + ((p_2[k]*N_j[t,k])/c_2[k])); // Eq 4.1   - Bev holt transition estimating survival from juvenile to spawner (plugs into Eq 4.4) 
   
   // kappa_marine[t,k] =  p_2[t,k]/ (1 + ((p_2[t,k]*N_j[t,k])/c_2[k])); // Eq 4.1   - Bev holt transition estimating survival from juvenile to spawner (plugs into Eq 4.4) 
   
    N_recruit[t,k] = kappa_marine[t,k]*N_j[t,k]; // Eq 4.5 generated estiamte for the amount of fish each year and stock that survive to a spawning stage
 
  //add in age for stage where I am tracking age class.... 
   // swtich to calendar years using t+A-a
      for (a in 1:A) { 
     N_returning[t+A-a,k,a] = N_recruit[t,k]*p[t+A-a,a]; // multiply recruits by Q to change scale, then distribute across age classes, p which is age comps. 

    // estimate harvest -- for later, for now H_b is not estimated as I try to get model to work.
    // N_sp[t+A-a,k,a] = (1-exp(-log_fm[i]))*N_returning[t+A-a,k,a];  // instantaneous fishing mortality * returning fish yields spawners. 
  
     N_sp[t+A-a,k,a] = N_returning[t+A-a,k,a]*(1-H_b[t+A-a,k,a]); // currently just feeding simulated harvest proportions until I get stuff to work    
    
     N_e[t+A-a,k,a] = fs[a,1]*Ps*N_sp[t+A-a,k,a]; // Eq 4.3 generated estimate for the amount of eggs produced that year for that stock.
      }
      // transition back to brood years - plug in ages manually
    N_e_sum[t,k] = (N_e[t+A-1,k,1]+N_e[t+A-2,k,2]+N_e[t+A-3,k,3]+N_e[t+A-4,k,4]);
    
   }
 }
   
  // Calculate age proportions by return year
  for(k in 1:K){
   for (t in 1:nRyrs) {
    for(a in 1:A){
      if(t< nByrs+1){
      q[t,k,a] = N_returning[t,k,a]/(sum(N_returning[t,k,1:A]));
      }
    }
  }
}

for(k in 1:K){
for(t in 1:nByrs){
 // translate juvenile fish to the appropriate scale 
 N_j_predicted[t,k]= catch_q[k,1]*N_j[t,k]; 
  }
 }

}// close block

model {
  // starting values for the population model 
for(k in 1:K) { 
  //start off with sigma fixed
   // sigma_y_j[k] ~  normal(1,10);// if i can get uncertainty from run reconstruction then i can fix this and not estiamte it -- currently fixed anyway for parsimony
   // sigma_y_sp[k] ~ normal(0,10);
   
   // Start priors for model parameters 
    log_catch_q[k,1] ~ normal(0,1); // Estimate Q - this will translate # of recruits to # of spawners 

    log_c_1[k,1] ~  normal(18.4, 10); // carrying capacity prior - stage 1  
    log_c_2[k,1] ~  normal(15, 10); // carrying capacity prior - stage 2
    
    log_p_1[k,1]~normal(-1.609438,1); // basal productivity estimate
    log_p_2[k,1]~normal(-0.9162907,1); // basal productivity estimate
}
  // log_fm ~ normal(0,2);    // instantaneous fishing mortality prior - not currenytly used, include harvest as known
     D_scale ~ beta(1,1);
// skip hierarchical covarite estimates for now, just have a prior on theta  
// for(c in 1:ncovars1){
//   	 mu_coef1[c] ~ normal(0.1, 1);  
//      //sigma_coef1[c] ~ normal(0, 10);  
//    }
// for(c in 1:ncovars2){
//   	 mu_coef2[c] ~ normal(-0.2, 1); 
//      //sigma_coef2[c] ~ normal(0, 10); 
//    }
//   	 
// for(k in 1:K) { 
//    for (c in 1:ncovars1) {
//       theta1[k,c] ~ normal(mu_coef1[c],sigma_coef1[c]);
//   }
//    for (c in 1:ncovars1) {
//       theta2[k,c] ~ normal(mu_coef2[c],sigma_coef2[c]);
//   }
// }

    // theta1[1]~normal(0.1,5); // environmental covariate coefficient stage 1
    // theta2[1]~normal(-0.2,10); // environmental covariate coefficient stage 2
    // 
 

 // age comp priors 
 // prob is fixed so I dont need this for now
  // prob[1] ~ beta(1,1);
  // prob[2] ~ beta(1,1);
  // prob[3] ~ beta(1,1);
 
   
// printing these for trouble shooting 
  // print("q: ", q)
  // print("p: ",  p)     
 
// Liklilihoods --  
  // Gamma variates for each year and age class which are used to determine age at maturity proportions
  for (y in 1:nRyrs) {
    for (a in 1:A) {
      //g[y,a] ~ gamma(Dir_alpha[a],1);
      target += gamma_lpdf(g[y,a]|Dir_alpha[a],1);
    }
  }

  // First `a.max` years of recruits, for which there is no spawner link
  // lnR[1:a_max] ~ normal(mean_ln_R0, sigma_R0);

  // State model -- no state space right now so currenlty not including this part 
  // lnR[(A+a_min):nRyrs] ~ normal(lnRm_2[(A+a_min):nRyrs], sigma_R);

  // Observation model
  for(k in 1:K){
  for (t in 2:nByrs) {
     log(data_stage_j[t,k]) ~ normal(log(N_j_predicted[t,k]), sigma_y_j[1,k]);
    } 
  }
  for(k in 1:K){ // stocks 
  for(t in 1:nRyrs){ // calendar years 
  // Currently ESS is fixed. 
  // for(a in 1:A){
    if(t<nByrs){
     // currently getting an error related to .* so removign this component from model temporarily
     // target += (ess_age_comp*sum(o_run_comp[t,1:A].*log(q[t,k,1:A])); // ESS_AGE_COMP right now is fixed
    target += normal_lpdf(log(data_stage_sp[t,k]) | log(sum(N_sp[t,k,1:A])), sigma_y_sp[1,k]);
    target += normal_lpdf(log(data_stage_return[t,k]) | log(sum(N_returning[t,k,1:A])), sigma_y_r[1,k]); // not sure if this is liklihood is right, returning here is escapement + harvest

    }
   }
  }
}  
 
// generated quantities {
//   real pp_log_N_j[nByrs,K]; // predicted recruits
//   real pp_log_N_sp[nRyrs,K,A]; // predicted spawners
//   //real pp_log_N_r[N-1,K];
// 
//  for (k in 1:K){
//    
//    for (t in 1:nByrs) {
//    pp_log_N_j[t,k] = (normal_rng(log(N_j[t,k]) - 0.5 * sigma_y_j[1,k]^2, sigma_y_j[1,k])); // generate posterior predictives with backtransform?
//     }
//    for (t in 1:nRyrs) {
//      for(a in 1:A){
//    pp_log_N_sp[t,k,a] = (normal_rng(log(N_sp[t,k,a]) - 0.5 * sigma_y_sp[1,k]^2, sigma_y_sp[1,k])); // generate posterior predictives with backtransform?
//     }
//    }
//  }
//  
// }
 
