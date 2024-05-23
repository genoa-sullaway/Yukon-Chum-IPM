data { // all equation references are from proposal numbering
  int<lower=0> nByrs ; // number of brood years  
  int<lower=0> nRyrs;  // Number of recruitment years in liklihood
  int<lower=0> nRyrs_T;  // Number of TOTAL recruitment years in SR model  
  int<lower=0> A;     // Number of age classes - 4
  int<lower=0> t_start;   // Number of age classes x2 for filling in starting values  
  
  real<lower=0> Ps; // Proportion of females in spawning stock, based on lit - currently 50%
  vector [A] fs; // fecundity
  vector [A-1] M; // fixed mortality for 3 older age classes
  vector [A] p_obs; // observed age structure for starting values... 

  vector[nByrs] data_stage_j;    // number of juveniles for each group  (basis)
  vector[nRyrs] data_stage_return;   //  number of harvest + escapement for each group 
  vector[nRyrs] data_stage_sp;   // number of spawners for each group (escapement)
  vector[nRyrs] data_stage_harvest;   // number of spawners for each group (escapement)
 
 // starting values for popualtion stages  
  // real N_sp_start [t_start,A];  
  // real N_catch_start [t_start,A];  
  real N_ocean_start[t_start,A];
  real N_egg_start [t_start,A];
  real N_j_start;
  //real N_recruit_start[t_start,A];
  real N_e_sum_start;

// kappa is marine and juvenile survival estimated via beverton holt transition fxn 
  real kappa_marine_start; // adding starting values for kappa so there arent NAs..not sure if this is necessary
  real kappa_j_start;
  
  int<lower=0> ncovars1; //number of covariates for first lifestage  
  int<lower=0> ncovars2; //number of covariates for second lifestage  

  matrix [nByrs, ncovars1] cov1; // covariate data in a matrix format 
  matrix [nByrs, ncovars2] cov2; // covariate data in a matrix format 
  
  matrix<lower=0, upper=1>[nRyrs,A] o_run_comp; // Observed age composition by year
  vector [nByrs] ess_age_comp;   // Effective input sample size for age comp "observations" -  currently fixed to 200 based on Hulson et al 2011
}
  
parameters {
real <lower=0 > log_c_1; // log carrying capacity
real <lower=0 > log_c_2; // log carrying capacity

// real <lower=10, upper=20>log_c_1; // log carrying capacity
// real <lower=10, upper=20>log_c_2; // log carrying capacity

//starting values 
 // vector [t_start] N_sp_start_log; 
 // vector [t_start]N_recruit_start_log;
 // vector [t_start]N_catch_start_log;
 //vector [t_start] N_ocean_start_log;
 
 vector<lower=12, upper=20> [t_start] N_sp_start_log;
 vector<lower=12>[t_start]N_recruit_start_log;
 vector<lower=5>[t_start]N_catch_start_log;
 //vector<lower=12>[t_start] N_ocean_start_log;

 //real<lower=13> N_j_start_log;
 //vector [t_start] N_egg_start_log;
//vector [t_start] N_egg_start_log;  
// real <lower=25> N_e_sum_start_log; 
// covariate parameters 
real theta1[ncovars1]; // covariate estimated for each covariate and each population 
real theta2[ncovars2];
  
vector <lower=0>[A-1] prob; 
real<lower=0.0001,upper=0.9> D_scale;     // Variability of age proportion vectors across cohorts
vector<lower=0> [A] g; // gamma random draws
real log_catch_q;
vector [nRyrs_T] log_F;
real basal_p_1; // mean alpha for covariate survival stage 1 
real basal_p_2; // mean alpha for covariate survival stage 2

real sigma_y_j;
real sigma_y_r;
real sigma_y_sp;
real sigma_y_h;

}

transformed parameters { 
 vector[nByrs] N_j; // predicted juveniles -calculated
 vector[nByrs] N_j_predicted; // predicted juveniles this goes into the liklihood- gets transformed by estimates of Q
 vector[nByrs] N_e_sum; // sum eggs across ages to then go into the lifecycle section that doesnt use age 
 
 //vector[t_start] N_egg_start; 
 real N_recruit [nRyrs_T,A]; 
 real N_sp [nRyrs_T,A];
 real N_catch [nRyrs_T,A];
 real N_ocean[nRyrs_T,A];
 real N_e [nRyrs_T,A];

real N_sp_start [t_start,A];
real N_recruit_start [t_start,A];
real N_catch_start [t_start,A];
//real N_ocean_start [t_start,A];
//real N_j_start;

//real N_egg_start [t_start, A];

// survival and covariate section 
vector <upper=1> [nByrs] p_1; // productivity in bev holt transition funciton, 1 = FW early marine 
vector <upper=1> [nByrs] p_2; // productivity in bev holt transition funciton, 1 = FW early marine 
 
vector [nByrs] kappa_j ; // predicted survival for juvenile fish (FW and early marine)
vector [nByrs] kappa_marine; // predicted survival for marine fish
vector [nByrs] kappa_marine_mortality; // converting kappa marine survival to mortality 

matrix [nByrs, ncovars1] cov_eff1; // array that holds FW and early marine covariate effects by brood year and stock
matrix [nByrs, ncovars2] cov_eff2; // array that holds FW and early marine covariate effects by brood year and stock
real <lower=0>  catch_q; // related juvebile data to spawner data (on different scales) gets transfomed from log to number 

real<lower=0> c_1; // estimate on log, transform back to normal scale 
real<lower=0> c_2; // estimate on log, transform back to normal scale 
  
// Age related transformed params ====== 
vector<lower=0>[A] p;  
real<lower=0> D_sum;                   // Inverse of D_scale which governs variability of age proportion vectors across cohorts
//vector [A] Dir_alpha;          // Dirichlet shape parameter for gamma distribution used to generate vector of age-at-maturity proportions
vector<lower=0>[A] Dir_alpha;          // Dirichlet shape parameter for gamma distribution used to generate vector of age-at-maturity proportions
matrix[nRyrs,A] q; 
vector<lower=0, upper=1> [A] pi;

vector [nRyrs_T] F; // instantaneous fishing mortality           

// starting value transformations ======
  kappa_marine[1] = kappa_marine_start; 
  kappa_j[1]= kappa_j_start; 
  N_j[1] = N_j_start; 
  N_e_sum[1]= N_e_sum_start; 
  
  //transform parameter - then add to pop vector
   // N_egg_start = exp(N_egg_start_log);
    
// real N_e_sum_start;
for(t in 1:t_start){
 for(a in 1:A){
  N_sp_start[t,a] = exp(N_sp_start_log[t]*p_obs[a]); 
  N_recruit_start[t,a] = exp(N_recruit_start_log[t]*p_obs[a]); 
 // N_ocean_start[t,a] = exp(N_ocean_start_log[t]*p_obs[a]);   
  N_catch_start[t,a] = exp(N_catch_start_log[t]*p_obs[a]); 
  } 
 }
 
 // N_j_start = exp(N_j_start_log);

    for(a in 1:A){
   // add starting values to the whole population array 
  N_recruit[1:t_start,a] = N_recruit_start[1:t_start,a];
  N_sp[1:t_start,a] = N_sp_start[1:t_start,a];
  N_catch[1:t_start,a] = N_catch_start[1:t_start,a];
  N_e[1:t_start,a] = N_egg_start[1:t_start,a]; 
  N_ocean[1:t_start,a] = N_ocean_start[1:t_start,a];
     }

  for(t in 1:nRyrs_T){
  // instant fishing mortality 
  F[t]  = exp(log_F[t]);
 }

  // transform log carrying capacity to normal scale
   c_1 = exp(log_c_1);
   c_2 = exp(log_c_2);
   
 // N_e_sum_start = exp(N_e_sum_start_log);
 // N_e_sum_start 
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
  pi[1] = prob[1];
  pi[2] = prob[2] * (1 - pi[1]);
  pi[3] = prob[3] * (1 - pi[1] - pi[2]);
  pi[4] = 1 - pi[1] - pi[2] - pi[3];

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
  
    // convert survival to mortality for next equation
    kappa_marine_mortality[t] = -log(kappa_marine[t]);
      for (a in 1:A) {  
        N_ocean[t+a,a] =  N_j[t]*p[a]; //convert to calendar year 
        
        if(a==1){
        N_recruit[t+a,a] = N_ocean[t+a,a]*exp(-kappa_marine_mortality[t]); // convert from survival to mortality 
           } 
        if(a>1){
        N_recruit[t+a,a] = N_ocean[t+a,a]*exp(-(sum(M[1:(a-1)])+kappa_marine_mortality[t])); // add age specific age mortality, kappa marine, survival in first winter gets put into the year 1 slot and then mortality is summer across larger age classes
           } 
        N_catch[t+a,a] = N_recruit[t+a,a]*(1-exp(-F[t+a]));
 
        N_sp[t+a,a] = N_recruit[t+a,a]-N_catch[t+a,a]; // fishing occurs before spawning -- 
         
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
  sigma_y_j ~ normal(0,8);
  sigma_y_r ~ normal(0,8);
  sigma_y_sp ~ normal(0,8);
  sigma_y_h ~ normal(0,8);
  //  sigma_y_j ~ normal(0,10); 
  // sigma_y_r ~ normal(0,10); 
  // sigma_y_sp ~ normal(0,10); 
  // sigma_y_h ~ normal(0,10); 
  
    log_catch_q ~ normal(-1.2,4); // Estimate Q - this will translate # of recruits to # of spawners 

    log_c_1 ~  normal(18, 10); // carrying capacity prior - stage 1  
    log_c_2 ~  normal(15, 10); // carrying capacity prior - stage 2

    N_sp_start_log ~ normal(10,10);
    N_recruit_start_log ~  normal(12.9, 10);
    N_catch_start_log ~ normal(8,10);//10.6
    //N_ocean_start_log ~ normal(14,10);
   // N_j_start_log ~ normal(15,5); 
    
    // N_egg_start_log ~ normal(30, 10);
    // N_e_sum_start_log ~  normal(30, 1); // starting value for eggs, initiates pop model 
    // N_e_sum_start_log ~  uniform(20, 30); // starting value for eggs, initiates pop model 
      // 
      // print("N_ocean_start_log:", N_ocean_start_log);
      // print("N_catch_start_log:", N_catch_start_log);
      // print("N_sp_start_log:", N_sp_start_log);
 
    // for (t in 1:t_start){
    //   N_egg_start_log[t] ~  normal(25, 5); 
    // }

    theta1[1] ~ normal(0,1); //normal(0.5,5); // environmental covariate coefficient stage 1
    theta1[2] ~ normal(0,1); // environmental covariate coefficient stage 1
 
    theta2[1] ~ normal(0,1); 
    theta2[2] ~ normal(0,1); // environmental covariate coefficient stage 1
   
    D_scale ~ beta(1,1); // 
    
    basal_p_1 ~ normal(0.5,1); // mean survival stage 1 
    basal_p_2 ~ normal(0.5,1); // mean survivial stage 2
    
    // basal_p_1 ~ normal(0,1); // mean survival stage 1 
    // basal_p_2 ~ normal(0,1); // mean survivial stage 2
    
// age comp 
    for (a in 1:A) {
   target += gamma_lpdf(g[a]|Dir_alpha[a],1);
 }
 
 // log fishing mortality for each calendar year 
  for(t in 1:nRyrs_T){
 log_F[t] ~ normal(-1.5,0.7); //  best I have gotten so far: -1.5,0.7);
 //normal(1,0.1); //-1.5,3);//log fishing mortatliy 0.1 penalizes toward the mean 
 
}

 // age comp priors -- maturity schedules
  prob[1] ~ beta(1,1); 
  prob[2] ~ beta(1,1);  
  prob[3] ~ beta(1,1);  
 
// printing these for trouble shooting 
  print("kappa_marine_mortality:", kappa_marine_mortality);
  print("log_F:", log_F);
  

// Likelilihoods --  
  // Observation model
  for (t in 1:nByrs) {
     log(data_stage_j[t]) ~ normal(log(N_j_predicted[t]), sigma_y_j);
    } 

  for(t in 1:nRyrs){ // calendar years 
 if(t<nByrs){
     target += ess_age_comp[t]*sum(o_run_comp[t,1:A] .* log(q[t,1:A])); // ESS_AGE_COMP right now is fixed
     
     target += normal_lpdf(log(data_stage_return[t]) | log(sum(N_recruit[t,1:A])), sigma_y_r); // not sure if this is liklihood is right, returning here is escapement + harvest
     target += normal_lpdf(log(data_stage_harvest[t]) | log(sum(N_catch[t,1:A])), sigma_y_h); // not sure if this is liklihood is right, returning here is escapement + harvest
     target += normal_lpdf(log(data_stage_sp[t]) |  log(sum(N_sp[t,1:A])), sigma_y_sp);
 }
  }
}  
