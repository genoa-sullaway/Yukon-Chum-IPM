data { // all equation references are from proposal numbering
  int<lower=0> nByrs ; // number of brood years  
  int<lower=0> nRyrs;  // Number of recruitment years in liklihood
  int<lower=0> nRyrs_T;  // Number of TOTAL recruitment years in SR model  
  int<lower=0> A;     // Number of age classes - 4
  int<lower=0> t_start;   // Number of age classes x2 for filling in starting values  
  int<lower=0> nByrs_return_dat; // brood years specifically for return data  
  int <lower=0> lik_count; // for generated quantities, how many likelihoods are there? 
  real<lower=0> Ps; // Proportion of females in spawning stock, based on lit - currently 50%
  // vector [A] alpha;
   // vector [A] fs; // fecundity
  vector [A] M; // fixed mortality for 3 older age classes
  real<lower=0> beta;
  real<lower=0> D_scale;

  vector[nByrs] juv_CV; 
  vector[nRyrs] return_CV; 

  vector[nByrs] data_stage_j;    // number of juveniles for each group  (basis)
  vector[nByrs_return_dat] data_stage_return;   //  number of harvest + escapement for each group 
  vector[nRyrs] data_stage_sp;   // number of spawners for each group (escapement)
  vector[nRyrs] data_stage_harvest;   // number of spawners for each group (escapement)
   
int<lower=0> ncovars1; //number of covariates for first lifestage  
int<lower=0> ncovars2; //number of covariates for second lifestage  

matrix [nByrs, ncovars1] cov1; // covariate data in a matrix format
matrix [nByrs, ncovars2] cov2; // covariate data in a matrix format
 
matrix<lower=0, upper=1>[nRyrs,A] o_run_comp; // Observed age composition by year
real ess_age_comp; 
}

transformed data {
    
// real<lower=0> c_1; // estimate on log, transform back to normal scale
// real<lower=0> c_2;

  // transform log carrying capacity to normal scale
// c_1 = exp(log_c_1);
// c_2 = exp(log_c_2);
    
  }
parameters {
 // starting values 
real <lower =0> N_j_start_log;
real <lower =0> N_brood_year_return_start_log;
real <lower =0> N_sp_start_log[t_start,A];
real <lower =0> N_recruit_start_log[t_start,A];
real <lower =0> N_catch_start_log[t_start,A];
real <lower =0> N_egg_start_log[t_start,A];

 real  log_c_1;
 real  log_c_2; // log carrying capacity

// covariate parameters 
real <lower =-1, upper = 1> theta1 [ncovars1]; // covariate estimated for each covariate and each population
real <lower =-1, upper = 1> theta2 [ncovars2];

// vector <lower=0> [A-1] prob;
// real <lower=0, upper=1> D_scale;     // Variability of age proportion vectors across cohorts
real <lower=0> g[nByrs,A]; // gamma random draws
// vector[3] prob;   // Maturity schedule probs
vector<lower=0,upper=1>[3] prob;   // Maturity schedule probs
  
real log_catch_q; 
real log_F_mean; 
vector [A] log_S; // log selectivity 
vector [nRyrs_T]  log_F_dev_y; 

real  basal_p_1; // mean prod for covariate survival stage 1
real  basal_p_2; // mean prod for covariate survival stage 2

// ricker parameters 
 vector <lower=1> [A] alpha;
 
 }

transformed parameters { 
 vector [nByrs] N_j; // predicted juveniles 
 vector [nByrs] N_j_predicted; // predicted juveniles this goes into the liklihood- gets transformed by estimates of Q
 vector [nByrs] N_e_sum; // sum eggs across ages to then go into the lifecycle section that doesnt use age 
 
 vector [nByrs] N_brood_year_return; // sum eggs across ages to then go into the lifecycle section that doesnt use age 

 real N_recruit [nRyrs_T,A]; 
 real N_sp [nRyrs_T,A];
 real N_catch [nRyrs_T,A]; 
 real N_e [nRyrs_T,A];
 
real N_sp_start [t_start,A];
real N_recruit_start [t_start,A];
real N_catch_start [t_start,A];
real N_egg_start[t_start,A];
real N_brood_year_return_start; 
real N_j_start; 
 
vector [nRyrs_T] F;
// vector <lower = 1> [A] alpha; 

vector [A] S; //selectivty
 
// survival and covariate section 
vector  <lower=0,upper = 1> [nByrs] p_1; // productivity in bev holt transition funciton, 1 = FW early marine
vector <lower=0,upper = 1>  [nByrs] p_2;
 
vector [nByrs] kappa_j_survival; // predicted survival for juvenile fish (FW and early marine)
vector [nByrs] kappa_marine_survival; // predicted survival for marine fish
 
matrix  [nByrs, ncovars1] cov_eff1; // array that holds FW and early marine covariate effects by brood year and stock
matrix  [nByrs, ncovars2] cov_eff2; 

real <lower=0>  catch_q; // related juvebile data to spawner data (on different scales) gets transfomed from log to number 
 
// Age related transformed params ====== 
matrix<lower=0, upper=1> [nByrs,A] p; // proportion of fish from each brood year that mature at a certain age
real <lower=0> D_sum;                   // Inverse of D_scale which governs variability of age proportion vectors across cohorts
vector <lower=0> [A] Dir_alpha;          // Dirichlet shape parameter for gamma distribution used to generate vector of age-at-maturity proportions
matrix  [nRyrs,A] q; 
vector <lower=0, upper=1> [A] pi; // actual age comps

real<lower=0> c_1; // estimate on log, transform back to normal scale
real<lower=0> c_2;
  
c_1 = exp(log_c_1);
c_2 = exp(log_c_2);
 
S = exp(log_S);
 
 // Eq 4.8 
  for(t in 1:nRyrs_T){
  // instant fishing mortality
  F[t]  = exp(log_F_mean + log_F_dev_y[t]);
 }
  
N_j_start = exp(N_j_start_log);
N_brood_year_return_start = exp(N_brood_year_return_start_log);

for(t in 1:t_start){
  for(a in 1:A){
  N_sp_start[t,a] = exp(N_sp_start_log[t,a]);
  N_recruit_start[t,a] = exp(N_recruit_start_log[t,a]);
  N_catch_start[t,a] = exp(N_catch_start_log[t,a]);
  N_egg_start[t,a] = exp(N_egg_start_log[t,a]);
  }
 }

  N_j[1] = N_j_start;
  N_brood_year_return[1] = N_brood_year_return_start;
 
    for(a in 1:A){
   // add starting values to the whole population array
  N_recruit[1:t_start,a] = N_recruit_start[1:t_start,a];
  N_sp[1:t_start,a] = N_sp_start[1:t_start,a];
  N_catch[1:t_start,a] = N_catch_start[1:t_start,a];
  N_e[1:t_start,a] = N_egg_start[1:t_start,a]; 
     }
  
  // Set up estimates of productivity with covariates
// the cov effects need seperate loop because number of covariates varies between lifestage (currently both 1 - eventually will vary)
  for(t in 1:nByrs){
   for (c in 1:ncovars1) {
  cov_eff1[t,c] =  theta1[c]*cov1[t,c]; // covariates for juveniles t+1 
   }
   for (c in 1:ncovars2) { 
  cov_eff2[t,c] =  theta2[c]*cov2[t,c];  
   }
  } 
  
  // Eq 4.3 - Part of Bev Holt transition funcitons 
for(t in 1:nByrs){
    p_1[t]  = 1 / (1 + exp(-basal_p_1-sum(cov_eff1[t,1:ncovars1])));
    p_2[t]  = 1 / (1 + exp(-basal_p_2- sum(cov_eff2[t,1:ncovars2])));
  }

// this is all part of Eq 4.6 
  pi[1] = prob[1];
  pi[2] = prob[2] * (1 - pi[1]);
  pi[3] = prob[3] * (1 - pi[1] - pi[2]);
  pi[4] = 1 - pi[1] - pi[2] - pi[3];
  
  D_sum = 1/(D_scale^2); 

  for (a in 1:A) {
    Dir_alpha[a] = D_sum * pi[a];
  for(t in 1:(nByrs)) {
    p[t,a] = g[t,a]/sum(g[t,1:A]);
   }
  }
 
catch_q = exp(log_catch_q); // Q to relate basis data to recruit/escapement data -- Is this right??

     for (t in 1:nByrs){ // loop for each brood year 
         N_e_sum[t] = sum(N_e[t,1:A]); // Eq. 4.11 
           
         kappa_j_survival[t] =  p_1[t]/(1 + ((p_1[t]*N_e_sum[t])/c_1)); // Eq 4.2
         
         N_j[t] = kappa_j_survival[t]*N_e_sum[t]; // Eq 4.1
     
         kappa_marine_survival[t] =  p_2[t]/(1 + ((p_2[t]*N_j[t])/c_2)); 
         
         N_brood_year_return[t] = N_j[t]*kappa_marine_survival[t]; // Eq 4.4          
        
        for (a in 1:A) { 
           N_recruit[t+a+1,a] = (N_brood_year_return[t]*p[t,a]) * exp(-(sum(M[1:a]))); //Eq 4.5 - part of it, see above for the rest of age comp estiamtion 

           // no selectivity 
           // N_catch[t+a+1,a] = N_recruit[t+a+1,a]*(1-exp(-(F[t+a+1]))); // Eq 4.7 
           // selectivity included 
           N_catch[t+a+1,a] = N_recruit[t+a+1,a]*(1-exp(-(F[t+a+1]*S[a]))); // Eq 4.7  
           
           N_sp[t+a+1,a] = N_recruit[t+a+1,a]-N_catch[t+a+1,a];  // Eq 4.9
           // issues estimating traditional ricker - changed to linear scalar
           
          N_e[t+a+1,a] = (Ps*N_sp[t+a+1,a]) * exp(alpha[a] - (beta * (Ps*N_sp[t+a+1,a])));
           // try linear and dont estimate beta
          //N_e[t+a+1,a] = (exp(alpha[a])) * (Ps*N_sp[t+a+1,a]); // Eq 4.10
          
            }
     }
     
  // Calculate age proportions by return year
  for (t in 1:nRyrs) {
    for(a in 1:A){
     q[t,a] = N_recruit[t,a]/(sum(N_recruit[t,1:A]));
    }
  }
  
for(t in 1:nByrs){
 // translate juvenile fish to the appropriate scale 
 N_j_predicted[t]= catch_q*N_j[t]; 
 }
}

model { 
   log_catch_q ~ normal(0,5); // works but loosening it up, normal(-5,1);

  log_S[1] ~ normal(0,1); 
  log_S[2] ~ normal(0,1); 
  log_S[3] ~ normal(0,1); 
  log_S[4] ~ normal(0,1); 
      
   alpha[1] ~  normal(0, 5); 
   alpha[2] ~  normal(0, 5);
   alpha[3] ~  normal(0, 5);
   alpha[4] ~  normal(0, 5);

   prob[1] ~ beta(1,1);
   prob[2] ~ beta(1,1);
   prob[3] ~ beta(1,1);
 
  log_c_1 ~ normal(15,2);
  log_c_2 ~ normal(17,2);

  // Dir_alpha ~ beta(1,1);

  theta1[1] ~ normal(0,0.1);
  theta1[2] ~ normal(0,0.1);
  theta1[3] ~ normal(0,0.1);
  theta1[4] ~ normal(0,0.1);
  
  theta2[1] ~ normal(0,0.1);
  theta2[2] ~ normal(0,0.1);
  theta2[3] ~ normal(0,0.1);
  
  // D_scale ~ beta(1,1);   

  basal_p_1 ~ normal(0,1.5);  
  basal_p_2 ~ normal(0,1.5); 

// age comp 
 for(t in 1:nByrs){
    for (a in 1:A) {
   target += gamma_lpdf(g[t,a]|Dir_alpha[a],1);
 }
}

// log fishing mortality for each calendar year
  log_F_mean ~ normal(0,0.1);
 
 for(t in 1:nRyrs_T){
   log_F_dev_y[t] ~ normal(0, 5);
}

 N_j_start_log ~ normal(14,5);
 N_brood_year_return_start_log~ normal(12,5);

 for(t in 1:t_start){
   for(a in 1:A){ 
    N_sp_start_log[t,a] ~ normal(3,5);
    N_recruit_start_log[t,a] ~  normal(3.2,5);
    N_catch_start_log[t,a] ~ normal(3,5);
    N_egg_start_log[t,a] ~  normal(6,5);
  }
 }    
   
 // Likelihoods - Observation model
  for (t in 1:nByrs) {
     target += normal_lpdf(log(data_stage_j[t]) | log(N_j_predicted[t]), (sqrt(log((0.2^2) + 1)))); //( sqrt(log((juv_CV[t]^2) + 1)))); //(sigma_juv + sqrt(log((juv_CV[t]^2) + 1))));//sqrt(log((0.38^2) + 1)));  // (sigma_juv));// + sqrt(log((juv_CV[t]^2) + 1)))); //sqrt(log((0.38^2) + 1)));
  }
 // recruit by brood year
  for (t in 1:nByrs_return_dat) {
    target += normal_lpdf(log(data_stage_return[t]) | log(N_brood_year_return[t]), (sqrt(log((0.07^2) + 1)))); // (sqrt(log((return_CV[t]^2) + 1)))); // sqrt(log((0.35^2) + 1))); // (sigma_rec));// + sqrt(log((return_CV[t]^2) + 1))));  //sqrt(log((0.35^2) + 1)));
    }

  for(t in 1:nRyrs){ // calendar years
     target +=  ess_age_comp*sum(o_run_comp[t,1:A] .* log(q[t,1:A])); // ESS_AGE_COMP right now is fixed

     target +=  normal_lpdf(log(data_stage_harvest[t]) | log(sum(N_catch[t,1:A])), (sqrt(log((0.07^2) + 1))));//( sqrt(log((return_CV[t]^2) + 1))));// sqrt(log((0.35^2) + 1)));  //
     target +=  normal_lpdf(log(data_stage_sp[t]) |  log(sum(N_sp[t,1:A])), (sqrt(log((0.07^2) + 1))));//(sqrt(log((return_CV[t]^2) + 1)))); //sqrt(log((0.35^2) + 1)));  //(sigma_sp));//
     }
  }

// generated quantities {
// // vector[nByrs] log_lik_j;                    // juvenile likelihood
// vector[nByrs_return_dat] log_lik_return;    // return likelihood
// vector[nRyrs] log_lik_age_comp;             // age composition likelihood
// vector[nRyrs] log_lik_harvest;              // harvest likelihood
// vector[nRyrs] log_lik_sp;                   // spawner likelihood
// 
// // // Juvenile likelihood
// // for (t in 1:nByrs) {
// //   log_lik_j[t] = normal_lpdf(log(data_stage_j[t]) | log(N_j_predicted[t]), (sigma_juv + sqrt(log((juv_CV[t]^2) + 1)))); // sqrt(log((0.39^2) + 1)));
// // }
// 
// // Return likelihood
// for (t in 1:nByrs_return_dat) {
//   log_lik_return[t] = normal_lpdf(log(data_stage_return[t]) | log(N_brood_year_return[t]), (sigma_rec + sqrt(log((return_CV[t]^2) + 1)))); //sqrt(log((0.35^2) + 1)));
// }
// 
// // Calendar year likelihoods
// for (t in 1:nRyrs) {
//   log_lik_age_comp[t] = ess_age_comp * sum(o_run_comp[t,1:A] .* log(q[t,1:A]));
//   log_lik_harvest[t] = normal_lpdf(log(data_stage_harvest[t]) | log(sum(N_catch[t,1:A])), (sigma_harvest + sqrt(log((return_CV[t]^2) + 1)))); // sqrt(log((0.35^2) + 1)));
//   log_lik_sp[t] = normal_lpdf(log(data_stage_sp[t]) | log(sum(N_sp[t,1:A])), (sigma_sp + sqrt(log((return_CV[t]^2) + 1)))); 
// }
// }


