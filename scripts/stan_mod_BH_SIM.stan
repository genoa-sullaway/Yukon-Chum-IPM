data { // all equation references are from proposal numbering
  int<lower=0> nByrs ; // number of brood years  
  int<lower=0> nRyrs;  // Number of recruitment years in SR model
  int<lower=0> A;     // Number of age classes - 4
  int<lower=0> t_start;   // Number of age classes x2 for filling in starting values  
  
  real<lower=0> Ps; // Proportion of females in spawning stock, based on lit - currently 50%
  vector [A] fs; 
  vector [A] M; // fixed age ocean mortality
  real basal_p_1;// mean alpha for covariate survival stage 1 
  real basal_p_2;// mean alpha for covariate survival stage 1 

  vector[nByrs] data_stage_j;    // number of juveniles for each group  (basis)
  vector[nRyrs] data_stage_return;   //  number of harvest + escapement for each group 
  vector[nRyrs] data_stage_sp;   // number of spawners for each group (escapement)
  real sigma_y_j;  // Initially fix sigma - process error for juveniles
  real sigma_y_r;  // Initially fix sigma - process error for returns
  real sigma_y_sp; // Initially fix sigma - process error for spawners

  vector<lower=0, upper=1>[A] pi; // Maturity schedule probabilities

   // starting values for popualtion stages  
  real N_sp_start [t_start,A];  
 // real N_returning_start [t_start,A];
  real N_ocean_start[t_start,A];
  real N_egg_start [t_start,A];
  real N_j_start;
  real N_recruit_start[t_start,A];
  real N_e_sum_start;

// kappa is marine and juvenile survival estimated via beverton holt transition fxn 
real kappa_marine_start; // adding starting values for kappa so there arent NAs..not sure if this is necessary
real kappa_j_start;
  
  int<lower=0> ncovars1; //number of covariates for first lifestage  
  int<lower=0> ncovars2; //number of covariates for second lifestage  
// 
//   real sigma_coef1[K,ncovars1]; // initially fix, error of the covariate effect across populations 
//   real sigma_coef2[K,ncovars2]; // initially fix, error of the covariate effect across populations 

  matrix [nByrs, ncovars1] cov1; // covariate data in a matrix format 
  matrix [nByrs, ncovars2] cov2; // covariate data in a matrix format 
  
   matrix<lower=0, upper=1>[nRyrs,A] o_run_comp; // Observed age composition by year
   vector [nByrs] ess_age_comp;   // Effective input sample size for age comp "observations" -  currently fixed to 200 based on Hulson et al 2011
}
  
parameters {
real <lower=10, upper=20>log_c_1; // log carrying capacity
real <lower=10, upper=20>log_c_2; // log carrying capacity
 
  // covariate parameters 
  real theta1[ncovars1]; // covariate estimated for each covariate and each population 
  real theta2[ncovars2];
  
real<lower=0,upper=0.5> D_scale;     // Variability of age proportion vectors across cohorts
vector<lower=0> [A] g; // gamma random draws
real<lower=0.001, upper=1> log_catch_q;
vector<lower=0> [A] log_fm; // instantaneous fishing mortality parameter  
//real<lower=0.0, upper=4.0> log_fm [nRyrs,A]; // instantaneous fishing mortality parameter  

}

transformed parameters { 
 vector[nByrs] N_j; // predicted juveniles -calculated
 vector[nByrs] N_j_predicted; // predicted juveniles this goes into the liklihood- gets transformed by estimates of Q
 vector[nByrs] N_e_sum; // sum eggs across ages to then go into the lifecycle section that doesnt use age 

 real N_recruit [nRyrs,A]; 
 real N_sp [nRyrs,A];
 real N_ocean[nRyrs,A];
 real N_e [nRyrs,A];

// survival and covariate section 
vector [nByrs] p_1; // productivity in bev holt transition funciton, 1 = FW early marine 
vector [nByrs] p_2; // productivity in bev holt transition funciton, 1 = FW early marine 
 
vector [nByrs] kappa_j ; // predicted survival for juvenile fish (FW and early marine)
vector [nByrs] kappa_marine; // predicted survival for marine fish
 
matrix [nByrs, ncovars1] cov_eff1; // array that holds FW and early marine covariate effects by brood year and stock
matrix [nByrs, ncovars2] cov_eff2; // array that holds FW and early marine covariate effects by brood year and stock
real <lower=0>  catch_q; // related juvebile data to spawner data (on different scales) gets transfomed from log to number 

real<lower=0> c_1; // estimate on log, transform back to normal scale 
real<lower=0> c_2; // estimate on log, transform back to normal scale 
  
//real<lower=0> p_1; // estimate on log, transform back to normal scale 
//real<lower=0> p_2; // estimate on log, transform back to normal scale 
  
// Age related transformed params ====== 
vector<lower=0>[A] p;  
real<lower=0> D_sum;                   // Inverse of D_scale which governs variability of age proportion vectors across cohorts
vector<lower=0>[A] Dir_alpha;          // Dirichlet shape parameter for gamma distribution used to generate vector of age-at-maturity proportions
matrix[nRyrs,A] q;

// starting value transformations ======
  kappa_marine[1] = kappa_marine_start; 
  kappa_j[1]= kappa_j_start; 
   // no age index on population stage
  N_j[1] = N_j_start; 
  
    for(a in 1:A){
   // add starting values to the whole population array 
  N_recruit[1:t_start,a] = N_recruit_start[1:t_start,a];
  N_sp[1:t_start,a] = N_sp_start[1:t_start,a];
  N_e[1:t_start,a] = N_egg_start[1:t_start,a]; 
  N_ocean[1:t_start,a] = N_ocean_start[1:t_start,a];
  N_e_sum[1] = N_e_sum_start;
     }
   
  // transform log carrying capacity to normal scale
   c_1 = exp(log_c_1);
   c_2 = exp(log_c_2);

// the cov effects need seperate loop because number of covariates varies between lifestage (currently both 1 - eventually will vary)
  for(t in 1:nByrs){
   for (c in 1:ncovars1) {
  cov_eff1[t,c] = theta1[c]*cov1[t,c];
   }
   for (c in 1:ncovars2) {
  cov_eff2[t,c] = theta2[c]*cov2[t,c];
   }
    p_1[t]  = 1 / (1 + exp(-basal_p_1- sum(cov_eff1[t,1:ncovars1])));
    p_2[t]  = 1 / (1 + exp(-basal_p_2- sum(cov_eff2[t,1:ncovars2])));
  }
 
 // Maturity schedule: use a common maturation schedule to draw the brood year specific schedules
 // currently fixing this by supplying prob in data section, but will likely eventually have it as a random variable.  
  // pi[1] = prob[1];  
  // pi[2] = prob[2] * (1 - pi[1]);
  // pi[3] = prob[3] * (1 - pi[1] - pi[2]);
  // pi[4] = 1 - pi[1] - pi[2] - pi[3];

  D_sum = 1/(D_scale^2);

  for (a in 1:A) {
    Dir_alpha[a] = D_sum * pi[a];
    p[a] = g[a]/sum(g[1:A]);
  }

catch_q = exp(log_catch_q); // Q to relate basis data to recruit/escapement data -- Is this right??

  for (t in 2:nByrs){ 
    kappa_j[t] =  p_1[t]/(1 + ((p_1[t]*N_e_sum[t-1])/c_1)); // Eq 4.1  - Bev holt transition estimating survival from Egg to Juvenile (plugs into Eq 4.4) 
     
    N_j[t] = kappa_j[t]*N_e_sum[t-1]; // Eq 4.4  generated estiamte for the amount of fish each year and stock that survive to a juvenile stage
   
    kappa_marine[t] =  p_2[t]/ (1 + ((p_2[t]*N_j[t])/c_2)); // Eq 4.1   - Bev holt transition estimating survival from juvenile to spawner (plugs into Eq 4.4) 
  
for (a in 1:A) {  
        N_ocean[t+a,a] =  N_j[t]*p[a]; // this still tracks on brood year
       
        N_recruit[t+a,a] = (kappa_marine[t]*N_ocean[t+a,a])*exp(-M[a]); ; // Eq 4.5 generated estiamte for the amount of fish each year and stock that survive to a spawning stage
      
        N_sp[t+a,a] = N_recruit[t+a,a]; //*(1-H_b[t+A-a,k,a]);

        N_e[t+a,a] = fs[a]*Ps*N_sp[t+a,a]; // Eq 4.3 generated estimate for the amount of eggs produced that year for that stock.
   }
      N_e_sum[t] = N_e[t,1] + N_e[t,2] + N_e[t,3] +N_e[t,4];
  }
   
  // Calculate age proportions by return year
  for (t in 1:nByrs) {
    for(a in 1:A){
     q[t,a] = N_ocean[t,a]/(sum(N_ocean[t,1:A]));
    }
  }

for(t in 1:nByrs){
 // translate juvenile fish to the appropriate scale 
 N_j_predicted[t]= catch_q*N_j[t]; 
 }
}

model {

  //start off with sigma fixed
   // sigma_y_j[k] ~  normal(1,10);// if i can get uncertainty from run reconstruction then i can fix this and not estiamte it -- currently fixed anyway for parsimony
   // sigma_y_sp[k] ~ normal(0,10);
   
   // Start priors for model parameters 
    log_catch_q ~ normal(0.26,0.05); // Estimate Q - this will translate # of recruits to # of spawners 

    log_c_1 ~  normal(18.4, 0.5); // carrying capacity prior - stage 1  
    log_c_2 ~  normal(17.5, 1); // carrying capacity prior - stage 2

    theta1[1]~normal(0.5,0.05); // environmental covariate coefficient stage 1
    theta1[2]~normal(-0.1,0.05); // environmental covariate coefficient stage 1
 
    theta2 ~ normal(-0.5,0.05); 
 
    // theta2[1] ~ normal(-1.5,0.05); 
    // theta2[2] ~ normal(-0.6,0.05); 
   
    D_scale ~ beta(0.3,0.001); // from simulation, will need to be 1,1 with full model 
 //D_scale ~ beta(1,1); // from simulation, will need to be 1,1 with full model 
  
// liklihood for age comp 
    for (a in 1:A) {
   target += gamma_lpdf(g[a]|Dir_alpha[a],1);
    log_fm ~ normal(0,2);    // instantaneous fishing mortality prior for each age
  }

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

   // theta2[1]~normal(-0.2,10); // environmental covariate coefficient stage 2
 
 // age comp priors -- maturity schedules

  // prob[1] ~ beta(1,1);
  // prob[2] ~ beta(1,1);
  // prob[3] ~ beta(1,1);
 
// printing these for trouble shooting 
 // print("N_ocean:", N_ocean)
 // print("p_1:", p_1)
 // print("p: ", p)
 // 

// Liklilihoods --  
 
  // Observation model
  for (t in 6:nByrs) {
     log(data_stage_j[t]) ~ normal(log(N_j_predicted[t]), sigma_y_j);
    } 

  
  for(t in 6:nRyrs){ // calendar years 
  // Currently ESS is fixed through time. 
  if(t<nByrs){
     target += ess_age_comp[t]*sum(o_run_comp[t,1:A] .* log(q[t,1:A])); // ESS_AGE_COMP right now is fixed
     
     target += normal_lpdf(log(data_stage_return[t]) | log(sum(N_recruit[t,1:A])), sigma_y_r); // not sure if this is liklihood is right, returning here is escapement + harvest
     target += normal_lpdf(log(data_stage_sp[t]) |  log(sum(N_sp[t,1:A])), sigma_y_sp);
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
 
