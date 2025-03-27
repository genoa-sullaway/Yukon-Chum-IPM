data { // all equation references are from proposal numbering
  int<lower=0> nByrs ; // number of brood years  
  int<lower=0> nRyrs;  // Number of recruitment years in liklihood
  int<lower=0> nRyrs_T;  // Number of TOTAL recruitment years in SR model  
  int<lower=0> A;     // Number of age classes - 4
  int<lower=0> t_start;   // Number of age classes x2 for filling in starting values  
  int<lower=0> nByrs_return_dat; // brood years specifically for return data  
  int <lower=0> lik_count; // for generated quantities, how many likelihoods are there? 
  real<lower=0> Ps; // Proportion of females in spawning stock, based on lit - currently 50%
  vector [A] fs; // fecundity
  vector [A] M; // fixed mortality for 3 older age classes

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
real <lower =10> N_j_start_log;
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
real <lower=0, upper=1> D_scale;     // Variability of age proportion vectors across cohorts
real <lower=0> g[nByrs,A]; // gamma random draws
vector <lower=0, upper=1> [A] pi; // actual age comps

real log_catch_q; 
real log_F_mean; 
// vector [A] log_S; // log selectivity 
vector [nRyrs_T]  log_F_dev_y; 

real <lower=0, upper = 1> basal_p_1; // mean prod for covariate survival stage 1
real <lower=0, upper = 1> basal_p_2; // mean prod for covariate survival stage 2

// ricker parameters 
vector <lower=0, upper = 15 > [A] alpha;
// real <lower=0> beta; 

// real <lower=0> sigma_juv;
// real <lower=0> sigma_rec;
// real <lower=0> sigma_harvest;
// real <lower=0> sigma_sp;
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
 
vector <lower = 0> [nRyrs_T] F;
// vector <lower = 0> [A] S; //selectivty

// survival and covariate section 
vector  <lower=0,upper = 1> [nByrs] p_1; // productivity in bev holt transition funciton, 1 = FW early marine
vector <lower=0,upper = 1>  [nByrs] p_2;
 
vector [nByrs] kappa_j_survival; // predicted survival for juvenile fish (FW and early marine)
vector [nByrs] kappa_marine_survival; // predicted survival for marine fish
 
matrix  [nByrs, ncovars1] cov_eff1; // array that holds FW and early marine covariate effects by brood year and stock
matrix  [nByrs, ncovars2] cov_eff2; 

real <lower=0>  catch_q; // related juvebile data to spawner data (on different scales) gets transfomed from log to number 

// alpha = exp(logalpha);
 
// Age related transformed params ====== 
matrix<lower=0, upper=1> [nByrs,A] p; // proportion of fish from each brood year that mature at a certain age
real<lower=0> D_sum;                   // Inverse of D_scale which governs variability of age proportion vectors across cohorts
vector<lower=0> [A] Dir_alpha;          // Dirichlet shape parameter for gamma distribution used to generate vector of age-at-maturity proportions
matrix  [nRyrs,A] q; 

real<lower=0> c_1; // estimate on log, transform back to normal scale
real<lower=0> c_2;
  
c_1 = exp(log_c_1);
c_2 = exp(log_c_2);
 
  // S = exp(log_S);
 
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
  
// the cov effects need seperate loop because number of covariates varies between lifestage (currently both 1 - eventually will vary)
  for(t in 1:nByrs){
   for (c in 1:ncovars1) {
  cov_eff1[t,c] =  theta1[c]*cov1[t,c]; // covariates for juveniles t+1 
   }
   for (c in 1:ncovars2) { 
  cov_eff2[t,c] =  theta2[c]*cov2[t,c];  
   }
  } 
  
for(t in 1:nByrs){
    p_1[t]  = 1 / (1 + exp(-basal_p_1-sum(cov_eff1[t,1:ncovars1])));
    p_2[t]  = 1 / (1 + exp(-basal_p_2- sum(cov_eff2[t,1:ncovars2])));
  }

  D_sum = 1/(D_scale^2);

  for (a in 1:A) {
    Dir_alpha[a] = D_sum * pi[a];
  for(t in 1:(nByrs)) {
    p[t,a] = g[t,a]/sum(g[t,1:A]);
   }
  }
 
catch_q = exp(log_catch_q); // Q to relate basis data to recruit/escapement data -- Is this right??

     for (t in 1:nByrs){ // loop for each brood year 
         N_e_sum[t] = sum(N_e[t,1:A]);
           
         kappa_j_survival[t] =  p_1[t]/(1 + ((p_1[t]*N_e_sum[t])/c_1)); // Eq 4.1  - Bev holt transition estimating survival from Egg to Juvenile (plugs into Eq 4.4) 

         N_j[t] = kappa_j_survival[t]*N_e_sum[t]; // Eq 4.4  generated estimate for the amount of fish each year and stock that survive to a juvenile stage
     
         kappa_marine_survival[t] =  p_2[t]/(1 + ((p_2[t]*N_j[t])/c_2)); //Eq 4.1  - Bev holt transition estimating survival from juvenile to spawner (plugs into Eq 4.4) 

         N_brood_year_return[t] = N_j[t]*kappa_marine_survival[t];         
        for (a in 1:A) { 
           N_recruit[t+a+1,a] = (N_brood_year_return[t]*p[t,a]); //add age specific mortality, 
           
           // N_catch[t+a+2,a] = N_recruit[t+a+2,a]*(1-exp(-(F[t+a+2]*S[a])));
           N_catch[t+a+1,a] = N_recruit[t+a+1,a]*(1-exp(-(F[t+a+1]))); 
           
           N_sp[t+a+1,a] = N_recruit[t+a+1,a]-N_catch[t+a+1,a];  
           
           // N_e[t+a+1,a] = Ps*(N_sp[t+a+1,a] * exp(alpha[a] - (beta * N_sp[t+a+1,a])));  
           // try linear and dont estimate beta
            N_e[t+a+1,a] = Ps*(exp(alpha[a]) * N_sp[t+a+1,a]);
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
   log_catch_q ~ normal(-5,1);
   
   for(a in 1:A){
      //alpha[a] ~  normal(7,5); // 
     alpha[a] ~ uniform(0,15);
   }
   
   // beta ~  normal(0.00001,0.01);

  pi ~ beta(1,1); 

  log_c_1 ~ normal(15,2);
  log_c_2 ~ normal(17,2);

  theta1[1] ~ normal(0,0.1);
  theta1[2] ~ normal(0,0.1);
  theta1[3] ~ normal(0,0.1);
  theta1[4] ~ normal(0,0.1);
  
  theta2[1] ~ normal(0,0.1);
  theta2[2] ~ normal(0,0.1);
  theta2[3] ~ normal(0,0.1);
  // theta2[4] ~ normal(0,0.1);
  
  D_scale ~ beta(1,1);  

  basal_p_1 ~ beta(1,1);  
  basal_p_2 ~ beta(1,1);  

// age comp 
 for(t in 1:nByrs){
    for (a in 1:A) {
   target += gamma_lpdf(g[t,a]|Dir_alpha[a],1);
 }
}

// log fishing mortality for each calendar year
log_F_mean ~ normal(0,0.1);
 
 for(t in 1:nRyrs_T){
   log_F_dev_y[t] ~ normal(0, 1);
}
 
 //sigmas ////
 // sigma_juv ~ normal(0, 0.2);
 // sigma_rec ~ normal(0, 0.2);
 // sigma_harvest ~ normal(0, 0.2);
 // sigma_sp ~ normal(0,0.2);

 // for (a in 1:A) {
 //    log_S[a] ~ normal(0,1);
 // } 

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
     target += normal_lpdf(log(data_stage_j[t]) | log(N_j_predicted[t]), ( sqrt(log((0.2^2) + 1)))); //( sqrt(log((juv_CV[t]^2) + 1)))); //(sigma_juv + sqrt(log((juv_CV[t]^2) + 1))));//sqrt(log((0.38^2) + 1)));  // (sigma_juv));// + sqrt(log((juv_CV[t]^2) + 1)))); //sqrt(log((0.38^2) + 1)));
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


