data { // all equation references are from proposal numbering
  int<lower=0> nByrs ; // number of brood years  
  int<lower=0> nRyrs;  // Number of recruitment years in SR model
  int<lower=0> A;     // Number of age classes - 4
  int<lower=0> t_start;   // Number of age classes x2 for filling in starting values  
  
  real<lower=0> Ps; // Proportion of females in spawning stock, based on lit - currently 50%
  vector [A] fs; 
  vector [A] M; // fixed age ocean mortality
  real basal_p_1;// mean alpha for covariate survival stage 1 

  vector[nByrs] data_stage_j;    // number of juveniles for each group  (basis)
  vector[nRyrs] data_stage_return;   //  number of harvest + escapement for each group 
  vector[nRyrs] data_stage_sp;   // number of spawners for each group (escapement)
  real sigma_y_j;  // Initially fix sigma - process error for juveniles
  real sigma_y_r;  // Initially fix sigma - process error for returns
  real sigma_y_sp; // Initially fix sigma - process error for spawners

  vector<lower=0, upper=1>[A] pi; // Maturity schedule probabilities

   // starting values for popualtion stages  
  real N_sp_start [t_start,A];  
  real N_returning_start [t_start,A];
  real N_ocean_start[t_start,A];
  real N_egg_start [t_start,A];
  real N_j_start;
  real N_recruit_start;
  real N_e_sum_start;

// kappa is marine and juvenile survival estimated via beverton holt transition fxn 
real kappa_marine_start; // adding starting values for kappa so there arent NAs..not sure if this is necessary
real kappa_j_start;
  
  int<lower=0> ncovars1; //number of covariates for first lifestage - starting with 1
  // int<lower=0> ncovars2; //number of covariates for second lifestage - starting with 1
// 
//   real sigma_coef1[K,ncovars1]; // initially fix, error of the covariate effect across populations 
//   real sigma_coef2[K,ncovars2]; // initially fix, error of the covariate effect across populations 

  matrix [nByrs, ncovars1] cov1; // covariate data in a matrix format 
  // real cov2[nByrs, ncovars2]; 
  
   matrix<lower=0, upper=1>[nRyrs,A] o_run_comp; // Observed age composition by year
   vector [nByrs] ess_age_comp;   // Effective input sample size for age comp "observations" -  currently fixed to 200 based on Hulson et al 2011
}
  
parameters {
real <lower=10, upper=20>log_c_1; // log carrying capacity
real <lower=10, upper=20>log_c_2; // log carrying capacity
 
  // covariate parameters 
  real theta1[ncovars1]; // covariate estimated for each covariate and each population 
  // real theta2[K,ncovars2];
  
 //real log_p_1; // covariate estimated for each covariate and each population 
 real log_p_2;

real<lower=0,upper=0.5> D_scale;     // Variability of age proportion vectors across cohorts
vector<lower=0> [A] g; // gamma random draws
real <lower=0.001, upper=1> log_catch_q;
// vector<lower=0.0, upper=4.0>[nRyrs] log_fm; // instantaneous fishing mortality parameter not used right now

  // real mu_coef1[ncovars1]; // mean covariate effect across populations (group level hierarchical effect)
  // real mu_coef2[ncovars2];
  // 
  // real sigma_coef1[ncovars1]; // error of the covariate effect across populations (group level hierarchical effect)
  // real sigma_coef2[ncovars2]; 
}

transformed parameters { 
 vector[nByrs] N_j; // predicted juveniles -calculated
 vector[nByrs] N_j_predicted; // predicted juveniles this goes into the liklihood- gets transformed by estimates of Q
 vector[nByrs] N_e_sum; // sum eggs across ages to then go into the lifecycle section that doesnt use age 
 vector[nByrs] N_recruit;  // predicted recruits - before they get assigned to returning age classes
 
 real N_sp [nRyrs,A];
 real N_returning[nRyrs,A];
 real N_ocean[nRyrs,A];
 real N_e [nRyrs,A];

// survival and covariate section 
vector [nByrs] p_1; // productivity in bev holt transition funciton, 1 = FW early marine 
// matrix[nByrs,K] p_2; // productivity in bev holt transition funciton, 2 = later marine 

vector [nByrs] kappa_j ; // predicted survival for juvenile fish (FW and early marine)
vector [nByrs] kappa_marine; // predicted survival for marine fish
 
matrix [nByrs, ncovars1] cov_eff1; // array that holds FW and early marine covariate effects by brood year and stock
// real cov_eff2[nByrs, K, ncovars2];// array that holds marine covariate effects by brood year and stock
real <lower=0>  catch_q; // related juvebile data to spawner data (on different scales) gets transfomed from log to number 

real<lower=0> c_1; // estimate on log, transform back to normal scale 
real<lower=0> c_2; // estimate on log, transform back to normal scale 
  
//real<lower=0> p_1; // estimate on log, transform back to normal scale 
real<lower=0> p_2; // estimate on log, transform back to normal scale 
  
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
  N_recruit[1] = N_recruit_start;
  
// for(t in 1:t_start){
    for(a in 1:A){
   // add starting values to the whole population array 
  N_sp[1:t_start,a] = N_sp_start[1:t_start,a];
  N_e[1:t_start,a] = N_egg_start[1:t_start,a]; 
  N_ocean[1:t_start,a] = N_ocean_start[1:t_start,a];
  N_returning[1:t_start,a] = N_returning_start[1:t_start,a]; 
  N_e_sum[1] = N_e_sum_start;
     }
   
  // transform log carrying capacity to normal scale
   c_1 = exp(log_c_1);
   c_2 = exp(log_c_2);
   
   //p_1 = exp(log_p_1);
   p_2 = exp(log_p_2);

// the cov effects need seperate loop because number of covariates varies between lifestage (currently both 1 - eventually will vary)

   for(t in 1:nByrs){
   for (c in 1:ncovars1) {
  cov_eff1[t,c] = theta1[c]*cov1[t,c];
   }
  }
 
//     for(t in 1:nByrs){
//      for (c in 1:ncovars2) {
//   cov_eff2[t,k,c] = theta2[k,c]*cov2[t,c];
//    }
//   }

// calculate productivity based on covariates for each lifestage 
 for(c in 1:ncovars1){
    for (t in 1:nByrs) { // this will need to be updated when adding more stocks and covariates to what is behind the slashes below
     p_1[t]  = 1 / (1 + exp(-basal_p_1- sum(cov_eff1[t,1:c])));
   }
  }

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
   
    kappa_marine[t] =  p_2/ (1 + ((p_2*N_j[t])/c_2)); // Eq 4.1   - Bev holt transition estimating survival from juvenile to spawner (plugs into Eq 4.4) 
   
    N_recruit[t] = kappa_marine[t]*N_j[t]; // Eq 4.5 generated estiamte for the amount of fish each year and stock that survive to a spawning stage
    
for (a in 1:A) {  
        N_ocean[t+a,a] =  N_recruit[t]*p[a]; // this still tracks on brood year
 
     // N_returning[t+1,1] =  N_recruit[t]*p[1]; // this still tracks on brood year
     // N_returning[t+2,2] =  N_recruit[t]*p[2]; // this still tracks on brood year
     // N_returning[t+3,3] =  N_recruit[t]*p[3]; // this still tracks on brood year
     // N_returning[t+4,4] =  N_recruit[t]*p[4]; // this still tracks on brood year
   
//now track on cal year - apply cal year specificy harvest, mortality, etc
//N_sp[t+a,a] = N_returning[t+a,a];//-100;//*(1-H_b[t+A-a,k,a]); // currently just feeding simulated harvest proportions until I get stuff to work    
         N_returning[t+a,a] = N_ocean[t+a,a]*exp(-M[a]); 
         N_sp[t+a,a] = N_returning[t+a,a];
  // for (a in 1:A) { 
      // N_sp[t+1,1] = N_returning[t+1,1];
      // N_sp[t+2,2] = N_returning[t+2,2];
      // N_sp[t+3,3] = N_returning[t+3,3];
      // N_sp[t+4,4] = N_returning[t+4,4];
      
      N_e[t+a,a] = fs[a]*Ps*N_sp[t+a,a]; // Eq 4.3 generated estimate for the amount of eggs produced that year for that stock.
      // N_e[t+2,2] = fs[2]*Ps*N_sp[t+2,2]; // Eq 4.3 generated estimate for the amount of eggs produced that year for that stock.
      // N_e[t+3,3] = fs[3]*Ps*N_sp[t+3,3]; // Eq 4.3 generated estimate for the amount of eggs produced that year for that stock.
      // N_e[t+4,4] = fs[4]*Ps*N_sp[t+4,4]; // Eq 4.3 generated estimate for the amount of eggs produced that year for that stock.

   }
      N_e_sum[t] = N_e[t,1] + N_e[t,2] + N_e[t,3] +N_e[t,4];
  }
   
  // Calculate age proportions by return year

  for (t in 1:nByrs) {
 for(a in 1:A){
   // if(t< nByrs){
     q[t,a] = N_ocean[t,a]/(sum(N_ocean[t,1:A]));
      // q[t,1] = N_returning[t,1]/sum(N_returning[t,1:A]);//new
      // q[t,2] = N_returning[t,2]/sum(N_returning[t,1:A]);//new
      // q[t,3] = N_returning[t,3]/sum(N_returning[t,1:A]);//new
      // q[t,4] = N_returning[t,4]/sum(N_returning[t,1:A]);//new
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
    log_c_2 ~  normal(17, 0.5); // carrying capacity prior - stage 2
    
   // log_p_1~normal(-1.6,5); // basal productivity estimate
    log_p_2~normal(-0.9,0.05); // basal productivity estimate

    theta1[1]~normal(0.5,0.05); // environmental covariate coefficient stage 1
    theta1[2]~normal(-0.1,0.05); // environmental covariate coefficient stage 1
 
    D_scale ~ beta(0.3,0.001); // from simulation, will need to be 1,1 with full model 
 //D_scale ~ beta(1,1); // from simulation, will need to be 1,1 with full model 
  
// liklihood for age comp 
    for (a in 1:A) {
    // g[a] ~ gamma(Dir_alpha[a],1);
   target += gamma_lpdf(g[a]|Dir_alpha[a],1);
  }
  //   for (a in 1:A) {
  //     for (t in 1:nRyrs) {
  //    // g[t,a] ~ gamma(Dir_alpha[a],1);
  //    target += gamma_lpdf(g[t,a]|Dir_alpha[a],1);
  //   }
  // }
  // log_fm ~ normal(0,2);    // instantaneous fishing mortality prior - not currenytly used, include harvest as known
         
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
 print("N_ocean:", N_ocean)
 print("p_1:", p_1)
 print("p: ", p)
 

// Liklilihoods --  
 
  // Observation model
  for (t in 6:nByrs) {
     log(data_stage_j[t]) ~ normal(log(N_j_predicted[t]), sigma_y_j);
    } 

  
  for(t in 6:nRyrs){ // calendar years 
  // Currently ESS is fixed through time. 
  if(t<nByrs){
     target += ess_age_comp[t]*sum(o_run_comp[t,1:A] .* log(q[t,1:A])); // ESS_AGE_COMP right now is fixed
     
     target += normal_lpdf(log(data_stage_return[t]) | log(sum(N_returning[t,1:A])), sigma_y_r); // not sure if this is liklihood is right, returning here is escapement + harvest
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
 
