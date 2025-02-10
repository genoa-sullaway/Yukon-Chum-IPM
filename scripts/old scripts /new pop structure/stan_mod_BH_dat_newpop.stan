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
 
  // vector[nRyrs] data_recruit_cv;// from run reconstruciton 
   //vector[nByrs] data_j_cv; 
   vector[nRyrs] data_sp_cv; // from run reconstruciton 
  
     
  real  basal_p_1_log;
  real  basal_p_2_log;
  //  real F; 
 // real basal_p_1; 
 // real basal_p_2; 
 
 // starting values for popualtion stages  
  // real N_sp_start [t_start,A];  
  // real N_catch_start [t_start,A];  
  // real N_ocean_start[t_start,A];
  // real N_egg_start [t_start,A];
  // real N_j_start;
  // //real N_recruit_start[t_start,A];
  // real N_e_sum_start;

// kappa is marine and juvenile survival estimated via beverton holt transition fxn 
  vector [2] kappa_marine_start; // adding starting values for kappa so there arent NAs..not sure if this is necessary
  real kappa_j_start;
  vector [2] kappa_marine_mort_start;
  
  int  ncovars1; //number of covariates for first lifestage  
  int  ncovars2; //number of covariates for second lifestage  
 
  matrix [nByrs,ncovars1] cov1; // covariate data in a matrix format 
  matrix [nByrs+1,ncovars2] cov2; // covariate data in a matrix format 
 
  // matrix [nByrs, ncovars1] cov1; // covariate data in a matrix format 
  // matrix [nByrs, ncovars2] cov2; // covariate data in a matrix format 
  
  matrix<lower=0, upper=1>[nRyrs,A] o_run_comp; // Observed age composition by year
  vector [nRyrs] ess_age_comp;   // Effective input sample size for age comp "observations" -  currently fixed to 200 based on Hulson et al 2011
}
  
parameters {
real <lower=13 > log_c_1; // log carrying capacity
real <lower=12 > log_c_2; // log carrying capacity
 
//starting values 
// vector <lower=5> [2] N_j_start_log; 
 // real<lower=5> N_j_start_log; 
 // real<lower=5> N_e_sum_start_log; 
 
 // vector [t_start]N_sp_start_log;
 // vector [t_start]N_recruit_start_log;
 // vector [t_start]N_catch_start_log;
 // vector [t_start]N_ocean_start_log;
 vector <lower=5>[t_start]N_egg_start_log; 
 
 // vector<lower=12, upper=20> [t_start] N_sp_start_log;
 // vector<lower=12>[t_start]N_recruit_start_log;
 // vector<lower=5>[t_start]N_catch_start_log;
 //vector<lower=12>[t_start] N_ocean_start_log;

 //real<lower=13> N_j_start_log;
 //vector [t_start] N_egg_start_log;
//vector [t_start] N_egg_start_log;  
// real <lower=25> N_e_sum_start_log; 
// covariate parameters 
// real <lower=-2, upper = 2> theta1[ncovars1]; // covariate estimated for each covariate and each population 
// real <lower=-2, upper = 2> theta2[ncovars2];
real  theta1[ncovars1]; // covariate estimated for each covariate and each population 
real  theta2[ncovars2];
  
vector <lower=0>[A-1] prob; 
real<lower=0.0001,upper=0.9> D_scale;     // Variability of age proportion vectors across cohorts
vector<lower=0> [A] g; // gamma random draws
real log_catch_q;
//vector<lower= -3>[nRyrs_T] log_F;
//vector<lower= -3>[19] log_F;
//vector [nRyrs_T] sigma_F_deviation;
vector [nRyrs_T] log_F_dev_y;
real log_F_mean;

// survival and covariate section 
// vector [nByrs] log_p_1; // productivity in bev holt transition funciton,  
//vector [nByrs] log_p_2; // productivity in bev holt transition funciton,  
//vector<lower=0, upper=1> [nByrs] p_2; // productivity in bev holt transition funciton,  
 
 
// real  basal_p_1; // mean alpha for covariate survival stage 1
// real  basal_p_2; // mean alpha for covariate survival stage 1

// real  basal_p_1_log; // mean alpha for covariate survival stage 1
// real  basal_p_2_log; // mean alpha for covariate survival stage 1
 // real  theta ; // mean alpha for covariate survival stage 2

 real sigma_y_j;
//real sigma_y_r;
//real sigma_y_sp;
//real sigma_y_h;
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

// real N_sp_start [t_start,A];
// real N_recruit_start [t_start,A];
// real N_catch_start [t_start,A];
// real N_ocean_start[t_start,A];
real N_egg_start[t_start,A];
// real N_j_start;
// real N_e_sum_start;

 real basal_p_1;
 real basal_p_2; 
 
vector [nByrs] kappa_j_survival ; // predicted survival for juvenile fish (FW and early marine)
vector  [nByrs+1] kappa_marine_survival; // predicted survival for marine fish
vector  [nByrs+1] kappa_marine_mortality; // converting kappa marine survival to mortality 

matrix [nByrs, ncovars1] cov_eff1; // array that holds FW and early marine covariate effects by brood year and stock
matrix [nByrs+1, ncovars2] cov_eff2; // array that holds FW and early marine covariate effects by brood year and stock
real <lower=0>  catch_q; // related juvebile data to spawner data (on different scales) gets transfomed from log to number 

real<lower=0> c_1; // estimate on log, transform back to normal scale 
real<lower=0> c_2; // estimate on log, transform back to normal scale 
  
vector [nByrs] p_1; // productivity in bev holt transition funciton,  
vector [nByrs+1] p_2; // productivity in bev holt transition funciton,  
 
// Age related transformed params ====== 
vector<lower=0>[A] p;  
real<lower=0> D_sum;                   // Inverse of D_scale which governs variability of age proportion vectors across cohorts
vector [A] Dir_alpha;          // Dirichlet shape parameter for gamma distribution used to generate vector of age-at-maturity proportions
//vector<lower=0>[A] Dir_alpha;          // Dirichlet shape parameter for gamma distribution used to generate vector of age-at-maturity proportions
matrix[nRyrs,A] q; 
// vector<lower=0, upper=1> [A] pi;
vector [A] pi;

//vector [19] F; //
vector [nRyrs_T] F; // instantaneous fishing mortality           

// starting value transformations ======
  kappa_marine_survival[1] = kappa_marine_start[1]; 
 // kappa_marine_mortality[1] = kappa_marine_mort_start; 
  kappa_j_survival[1]= kappa_j_start; 
 
for(t in 1:t_start){
 for(a in 1:A){
  // N_sp_start[t,a] = exp(N_sp_start_log[t])*p_obs[a]; 
  // N_recruit_start[t,a] = exp(N_recruit_start_log[t])*p_obs[a]; 
  // N_ocean_start[t,a] = exp(N_ocean_start_log[t])*p_obs[a];   
  // N_catch_start[t,a] = exp(N_catch_start_log[t])*p_obs[a]; 
  N_egg_start[t,a] = exp(N_egg_start_log[t])*p_obs[a];  
  } 
 }
 
 // N_j_start = exp(N_j_start_log);
 // N_j[1] = N_j_start;
 
 // N_e_sum_start = exp(N_e_sum_start_log);
 // N_e_sum[1] = N_e_sum_start;
 
    for(a in 1:A){
   // add starting values to the whole population array 
  // N_recruit[1:t_start,a] = N_recruit_start[1:t_start,a];
  // N_sp[1:t_start,a] = N_sp_start[1:t_start,a];
  // N_catch[1:t_start,a] = N_catch_start[1:t_start,a];
  N_e[1:t_start,a] = N_egg_start[1:t_start,a]; 
  // N_ocean[1:t_start,a] = N_ocean_start[1:t_start,a];
     }
 
 basal_p_1 = exp(basal_p_1_log);
 basal_p_2 = exp(basal_p_2_log);
 
 // for(t in 1:19){//  
  for(t in 1:nRyrs_T){
  // instant fishing mortality
  F[t]  = exp(log_F_mean +log_F_dev_y[t]);
 // F[t]  = exp(log_F[t]);
 }
 
  // transform log carrying capacity to normal scale
   c_1 = exp(log_c_1);
   c_2 = exp(log_c_2);
   
   // p_1 = exp(log_p_1);
  // p_2 = exp(log_p_2);
 
//the cov effects need seperate loop because number of covariates varies between lifestage (currently both 1 - eventually will vary)
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

catch_q = exp(log_catch_q); // Q to relate basis data to recruit/escapement data 

  for (t in 1:nByrs){ 
    N_e_sum[t] = N_e[t,1] + N_e[t,2] + N_e[t,3] +N_e[t,4];
    
    kappa_j_survival[t] =  p_1[t]/(1 + ((p_1[t]*N_e_sum[t])/c_1)); // Eq 4.1  - Bev holt transition estimating survival from Egg to Juvenile (plugs into Eq 4.4) 
     
    N_j[t] = kappa_j_survival[t]*N_e_sum[t]; // Eq 4.4  generated estiamte for the amount of fish each year and stock that survive to a juvenile stage
   
    kappa_marine_survival[t+1] =  p_2[t+1]/(1 + ((p_2[t+1]*N_j[t])/c_2)); // Eq 4.1   - Bev holt transition estimating survival from juvenile to spawner (plugs into Eq 4.4) 
  
    // convert survival to mortality for next equation
    kappa_marine_mortality[t+1] = -log(kappa_marine_survival[t+1]);
    
      for (a in 1:A) {  
        N_ocean[t+a+1,a] =  N_j[t]*p[a]; //convert to calendar year 
        
        if(a==1){
        N_recruit[t+a+1,a] = N_ocean[t+a+1,a]*exp(-kappa_marine_mortality[t+1]); // convert from survival to mortality 
           } 
        if(a>1){
        N_recruit[t+a+1,a] = N_ocean[t+a+1,a]*exp(-(sum(M[1:(a-1)])+kappa_marine_mortality[t+1])); // add age specific age mortality, kappa marine, survival in first winter gets put into the year 1 slot and then mortality is summer across larger age classes
           } 
       // if(t+a<20){
        N_catch[t+a+1,a] = N_recruit[t+a+1,a]*(1-(exp(-F[t+a+1])));
        //   }
       //     if (t+a>19){
       //       N_catch[t+a,a] = N_recruit[t+a,a]*(1-exp(-0.02));
       // }
       
        N_sp[t+a+1,a] = N_recruit[t+a+1,a]-N_catch[t+a+1,a]; // fishing occurs before spawning -- 
         
        N_e[t+a+1,a] = fs[a]*Ps*N_sp[t+a+1,a]; // Eq 4.3 generated estimate for the amount of eggs produced that year for that stock.
   } // N_e gets a plus one because eggs would start a new brood year
      
  }
   
  // Calculate age proportions by return year
  for (t in 1:nByrs) {
    for(a in 1:A){
     q[t,a] = N_sp[t,a]/(sum(N_sp[t,1:A]));
    }
  }

for(t in 1:nByrs){
 // translate juvenile fish to the appropriate scale 
 N_j_predicted[t]= catch_q*N_j[t]; 
 }
}

model {
  sigma_y_j ~ uniform(0,5); //normal(0,8);
  //sigma_y_r ~ normal(0,8);
  //sigma_y_sp ~ uniform(0,5);//normal(0,8);
  //sigma_y_h ~ normal(0,8);
  //  sigma_y_j ~ normal(0,10); 
  // sigma_y_r ~ normal(0,10); 
  // sigma_y_sp ~ normal(0,10); 
  // sigma_y_h ~ normal(0,10); 
  
    log_catch_q ~ normal(0,5);//normal(-1.2,4); // Estimate Q - this will translate # of recruits to # of spawners 

    log_c_1 ~  normal(20, 6); // carrying capacity prior - stage 1  
    log_c_2 ~  normal(16, 6); // carrying capacity prior - stage 2

    // log_c_1 ~  normal(18, 10); // carrying capacity prior - stage 1  
    // log_c_2 ~  normal(15, 10); // carrying capacity prior - stage 2

    // N_j_start_log ~ normal(13.6,5); //13.8
    // N_e_sum_start_log ~  normal(13.65, 5); //normal(14, 8);  starting value for eggs, initiates pop model 
    
 for(t in 1:t_start){
    // N_sp_start_log[t] ~ normal(13.48,5);
    // N_recruit_start_log[t] ~  normal(13.5,5);
    // N_catch_start_log[t] ~ normal(12.3,5);
    // N_ocean_start_log[t] ~ normal(13.55,5);
    N_egg_start_log[t] ~  normal(14, 5); // starting value for eggs, initiates pop model 
}
  //print("N_e_sum_start_log:", N_e_sum_start_log);
 
    theta1[1]  ~ normal(0,1.5); //normal(0.5,5); // environmental covariate coefficient stage 1
    // theta1[2] ~ normal(0,1.5); // environmental covariate coefficient stage 1
    // theta1[3]  ~ normal(0,1.5); //normal(0.5,5); // environmental covariate coefficient stage 1
    // theta1[4] ~ normal(0,1.5); // environmental covariate coefficient stage 1
 
    theta2[1]  ~ normal(0,1.5);
    // theta2[2] ~ normal(0,1.5); // environmental covariate coefficient stage 1
    // theta2[3]  ~ normal(0,1.5); //normal(0.5,5); // environmental covariate coefficient stage 1
    // 
    D_scale ~ beta(1,1); // 
    
   // basal_p_1 ~ normal(0,1); // currys model: normal(0,1.5^2); // mean survival stage 1
   // basal_p_2 ~ normal(0,1); // me
   
   // log_p_1 ~ normal(0,1); // currys model: normal(0,1.5^2); // mean survival stage 1
  // log_p_2 ~ normal(0,1); // me
   //   p_2 ~ beta(1,1); // me
   // basal_p_1_log ~ normal(0,1); // currys model: normal(0,1.5^2); // mean survival stage 1
   // basal_p_2_log ~ normal(0,1); // mean survivial stage 2
   //  
    // currys model: rnorm(1, 0,1.5^2)
    // simulation:  basal_p_1 ~ normal(0.1,0.5);
    // before: normal(0.5,1)
    
    // basal_p_1 ~ normal(0,1); // mean survival stage 1 
    // basal_p_2 ~ normal(0,1); // mean survivial stage 2
    
// age comp 
    for (a in 1:A) {
   target += gamma_lpdf(g[a]|Dir_alpha[a],1);
 }
 
 // log fishing mortality for each calendar year 
log_F_mean ~ normal(-1,1); //uniform(-2,-0.5);  //

//for(t in 1:19){ //
for(t in 1:nRyrs_T){
  //sigma_F_deviation[t] ~ normal(5,0.1);
// log_F_dev_y[t] ~ normal(0,sigma_F_deviation[t]);
 //log_F_dev_y[t] ~ normal(0,5);
 log_F_dev_y[t] ~ normal(0,1); // sigma_F_deviation[t]);
 //log_F[t] ~ normal(1,3); // 1,2 does p good, 1,3 does better normal(-1.5,0.7);
 // log_F[t] ~ normal(-1.5,0.7); //  best have gotten so far: -1.5,0.7);
 // normal(1,0.1); //-1.5,3);//log fishing mortatliy 0.1 penalizes toward the mean
}
 
 // age comp priors -- maturity schedules
  prob[1] ~ beta(1,1); 
  prob[2] ~ beta(1,1);  
  prob[3] ~ beta(1,1);  
 
// printing these for trouble shooting 
// print("kappa_marine_mortality:", kappa_marine_mortality);
// print("kappa_marine_survival:", kappa_marine_survival);
  // print("log_F:", log_F);
   
// Likelilihoods --  
  // Observation model
  for (t in 1:nByrs) {
     log(data_stage_j[t]) ~ normal(log(N_j_predicted[t]), sigma_y_j); //sqrt(log((0.06^2) + 1))); //sigma_y_j); // sqrt(log((data_j_cv[t]) + 1)));//sigma_y_j); //sqrt(log((0.08^2) + 1)));
    } 

  for(t in 1:nRyrs){ // calendar years 
 //if(t<nByrs){
     target += ess_age_comp[t]*sum(o_run_comp[t,1:A] .* log(q[t,1:A])); // ESS_AGE_COMP right now is fixed
     
     target += normal_lpdf(log(data_stage_return[t]) | log(sum(N_recruit[t,1:A])), sqrt(log((data_sp_cv[t]^2) + 1))); // sqrt(log((0.06^2) + 1))); // not sure if this is liklihood is right, returning here is escapement + harvest
     target += normal_lpdf(log(data_stage_harvest[t]) | log(sum(N_catch[t,1:A])), sqrt(log((0.01^2) + 1))); // not sure if this is liklihood is right, returning here is escapement + harvest
     target += normal_lpdf(log(data_stage_sp[t]) |  log(sum(N_sp[t,1:A])), sqrt(log((data_sp_cv[t]^2) + 1))); // sigma_y_sp);
 //}
  }
}  
