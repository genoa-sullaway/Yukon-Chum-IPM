data { // all equation references are from proposal numbering
  int<lower=0> nByrs ; // number of brood years  
  int<lower=0> nRyrs;  // Number of recruitment years in liklihood
  int<lower=0> nRyrs_T;  // Number of TOTAL recruitment years in SR model  
  int<lower=0> A;     // Number of age classes - 4
  int<lower=0> t_start;   // Number of age classes x2 for filling in starting values  
  
  real<lower=0> Ps; // Proportion of females in spawning stock, based on lit - currently 50%
  vector [A] fs; // fecundity
  vector [A] M; // fixed mortality for 3 older age classes
  vector [A] p_obs; // observed age structure for starting values... 

  vector[nByrs] data_stage_j;    // number of juveniles for each group  (basis)
  vector[nRyrs] data_stage_return;   //  number of harvest + escapement for each group 
  vector[nRyrs] data_stage_sp;   // number of spawners for each group (escapement)
  vector[nRyrs] data_stage_harvest;   // number of spawners for each group (escapement)

real kappa_marine_start; // adding starting values for kappa so there arent NAs..not sure if this is necessary
real kappa_j_start;
real kappa_marine_mort_start;

vector[nRyrs] data_recruit_cv; 
vector[nRyrs] data_sp_cv; // from run reconstruciton 

int<lower=0> ncovars1; //number of covariates for first lifestage  
int<lower=0> ncovars2; //number of covariates for second lifestage  

matrix [nByrs+1, ncovars1] cov1; // covariate data in a matrix format
matrix [nByrs+2, ncovars2] cov2; // covariate data in a matrix format
 
matrix<lower=0, upper=1>[nRyrs,A] o_run_comp; // Observed age composition by year
vector [nByrs] ess_age_comp;   // Effective input sample size for age comp "observations" -  currently fixed to 200 based on Hulson et al 2011
}
  
parameters {
 // real <lower=5, upper = 25> log_c_1; // log carrying capacity
 // real <lower=5, upper = 25> log_c_2; // log carrying capacity
 real  log_c_1;
 real  log_c_2; // log carrying capacity

// starting values 
 real <lower= 0> N_j_start_log; 
 // real<lower=5, upper=18> N_e_sum_start_log; 

 vector <lower= 0> [t_start] N_first_winter_start_log;
 vector <lower= 0> [t_start] N_sp_start_log;
 vector <lower= 0> [t_start] N_recruit_start_log;
 vector <lower= 0> [t_start] N_catch_start_log;
 vector <lower= 0> [t_start] N_egg_start_log;

// covariate parameters 
real <lower= -2, upper = 2> theta1 [ncovars1]; // covariate estimated for each covariate and each population
real <lower= -2, upper = 2> theta2 [ncovars2];

vector <lower=0> [A-1] prob; 
real <lower=0.1, upper=0.9> D_scale;     // Variability of age proportion vectors across cohorts
vector <lower=0> [A] g; // gamma random draws
real log_catch_q;
 
vector [nRyrs_T]  log_F_dev_y; 
real log_F_mean; 

real <lower=0.1, upper = 0.9> basal_p_1; // mean alpha for covariate survival stage 1
real <lower=0.1, upper = 0.9> basal_p_2; // mean alpha for covariate survival stage 2

 // real sigma_y_j;
}

transformed parameters { 
 vector[nByrs+1] N_j; // predicted juveniles 
 vector[nByrs+1] N_j_predicted; // predicted juveniles this goes into the liklihood- gets transformed by estimates of Q
 vector[nByrs+1] N_e_sum; // sum eggs across ages to then go into the lifecycle section that doesnt use age 

 real N_first_winter [nRyrs_T+2,A]; 
 real N_recruit [nRyrs_T+2,A]; 
 real N_sp [nRyrs_T+2,A];
 real N_catch [nRyrs_T+2,A];
 real N_ocean[nRyrs_T+2,A];
 real N_e [nRyrs_T+2,A];

real N_sp_start [t_start,A];
real N_recruit_start [t_start,A];
real N_catch_start [t_start,A];
real N_egg_start[t_start,A];
real N_first_winter_start[t_start,A];
real N_j_start;
// real N_e_sum_start;
real c_1;
real c_2;
// survival and covariate section 
vector   [nByrs+1] p_1; // productivity in bev holt transition funciton, 1 = FW early marine
vector   [nByrs+2] p_2;

// vector <lower=0.001, upper = 0.99> [nByrs+1] p_1; // productivity in bev holt transition funciton, 1 = FW early marine
// vector <lower=0.001, upper = 0.99> [nByrs+1+1] p_2;

vector [nByrs+1] kappa_j_survival; // predicted survival for juvenile fish (FW and early marine)
//vector <lower=0.001, upper = 0.5> [nByrs] kappa_j_survival; // predicted survival for juvenile fish (FW and early marine)
vector [nByrs+2] kappa_marine_survival; // predicted survival for marine fish
vector [nByrs+2] kappa_marine_mortality; // converting kappa marine survival to mortality 

matrix [nByrs+1, ncovars1] cov_eff1; // array that holds FW and early marine covariate effects by brood year and stock
matrix [nByrs+2, ncovars2] cov_eff2; 

real <lower=0>  catch_q; // related juvebile data to spawner data (on different scales) gets transfomed from log to number 
 
// Age related transformed params ====== 
vector<lower=0.001>[A] p;  
real<lower=0.001> D_sum;                   // Inverse of D_scale which governs variability of age proportion vectors across cohorts
vector<lower=0.001>[A] Dir_alpha;          // Dirichlet shape parameter for gamma distribution used to generate vector of age-at-maturity proportions
matrix[nRyrs,A] q; 
vector<lower=0, upper=1> [A] pi;

vector [nRyrs_T] F; // instantaneous fishing mortality           

// starting value transformations ======
  // kappa_marine_survival[1] = kappa_marine_start;
  // kappa_marine_mortality[1] = kappa_marine_mort_start;
  // kappa_j_survival[1]= kappa_j_start;
 
  for(t in 1:nRyrs_T){//  
  // instant fishing mortality 
  F[t]  = exp(log_F_mean +log_F_dev_y[t]);
 }
 
for(t in 1:t_start){
 for(a in 1:A){
  N_sp_start[t,a] = exp(N_sp_start_log[t])*p_obs[a];
  N_recruit_start[t,a] = exp(N_recruit_start_log[t])*p_obs[a];
  N_catch_start[t,a] = exp(N_catch_start_log[t])*p_obs[a];
  N_egg_start[t,a] = exp(N_egg_start_log[t])*p_obs[a];
  N_first_winter_start[t,a] = exp(N_first_winter_start_log[t])*p_obs[a];
  }
 }

// try adding starting values specific to ages ....
// for(a in 1:A){
//   if(a==1){
//    for(t in 1:3){
//       N_sp_start[t,a] = exp(N_sp_start_log[t])*p_obs[a];
//       N_recruit_start[t,a] = exp(N_recruit_start_log[t])*p_obs[a];
//       // N_ocean_start[t,a] = exp(N_ocean_start_log[t])*p_obs[a];
//       N_catch_start[t,a] = exp(N_catch_start_log[t])*p_obs[a];
//      N_egg_start[t,a] = exp(N_egg_start_log[t])*p_obs[a];
//    }
//   }
//    if(a==2){
//    for(t in 1:4){
//      N_sp_start[t,a] = exp(N_sp_start_log[t])*p_obs[a];
//      N_recruit_start[t,a] = exp(N_recruit_start_log[t])*p_obs[a];
//      // N_ocean_start[t,a] = exp(N_ocean_start_log[t])*p_obs[a];
//      N_catch_start[t,a] = exp(N_catch_start_log[t])*p_obs[a];
//      N_egg_start[t,a] = exp(N_egg_start_log[t])*p_obs[a];
//     }
//    }
//     if(a==3){
//    for(t in 1:5){
//       N_sp_start[t,a] = exp(N_sp_start_log[t])*p_obs[a];
//       N_recruit_start[t,a] = exp(N_recruit_start_log[t])*p_obs[a];
//       // N_ocean_start[t,a] = exp(N_ocean_start_log[t])*p_obs[a];
//       N_catch_start[t,a] = exp(N_catch_start_log[t])*p_obs[a];
//       N_egg_start[t,a] = exp(N_egg_start_log[t])*p_obs[a];
//    }
//     }
// 
//     if(a==4){
//    for(t in 1:6){
//       N_sp_start[t,a] = exp(N_sp_start_log[t])*p_obs[a];
//       N_recruit_start[t,a] = exp(N_recruit_start_log[t])*p_obs[a];
//       // N_ocean_start[t,a] = exp(N_ocean_start_log[t])*p_obs[a];
//       N_catch_start[t,a] = exp(N_catch_start_log[t])*p_obs[a];
//       N_egg_start[t,a] = exp(N_egg_start_log[t])*p_obs[a];
//     }
//    }
//   }

 
 N_j_start = exp(N_j_start_log);
 N_j[1] = N_j_start;
 
    for(a in 1:A){
   // add starting values to the whole population array
  N_recruit[1:t_start,a] = N_recruit_start[1:t_start,a];
  N_sp[1:t_start,a] = N_sp_start[1:t_start,a];
  N_catch[1:t_start,a] = N_catch_start[1:t_start,a];
  N_e[1:t_start,a] = N_egg_start[1:t_start,a];
  N_first_winter[1:t_start,a] = N_first_winter_start[1:t_start,a];
     }
//   
// for(a in 1:A){
//   if(a==1){
//    for(t in 1:3){
//       N_recruit[1:t_start,a] = N_recruit_start[1:t_start,a];
//       N_sp[1:t_start,a] = N_sp_start[1:t_start,a];
//       N_catch[1:t_start,a] = N_catch_start[1:t_start,a];
//       N_e[1:t_start,a] = N_egg_start[1:t_start,a];
//       // N_ocean[1:t_start,a] = N_ocean_start[1:t_start,a];
//    }
//   }
//    if(a==2){
//    for(t in 1:4){
//      N_recruit[1:t_start,a] = N_recruit_start[1:t_start,a];
//       N_sp[1:t_start,a] = N_sp_start[1:t_start,a];
//       N_catch[1:t_start,a] = N_catch_start[1:t_start,a];
//       N_e[1:t_start,a] = N_egg_start[1:t_start,a];
//       // N_ocean[1:t_start,a] = N_ocean_start[1:t_start,a];
//    }
//    }
//     if(a==3){
//    for(t in 1:5){
//       N_recruit[1:t_start,a] = N_recruit_start[1:t_start,a];
//       N_sp[1:t_start,a] = N_sp_start[1:t_start,a];
//       N_catch[1:t_start,a] = N_catch_start[1:t_start,a];
//       N_e[1:t_start,a] = N_egg_start[1:t_start,a];
//       // N_ocean[1:t_start,a] = N_ocean_start[1:t_start,a];
//    }
//     }
//     if(a==4){
//    for(t in 1:6){
//       N_recruit[1:t_start,a] = N_recruit_start[1:t_start,a];
//       N_sp[1:t_start,a] = N_sp_start[1:t_start,a];
//       N_catch[1:t_start,a] = N_catch_start[1:t_start,a];
//       N_e[1:t_start,a] = N_egg_start[1:t_start,a];
//       // N_ocean[1:t_start,a] = N_ocean_start[1:t_start,a];
//    }
//    }
//   }

  // transform log carrying capacity to normal scale
   c_1 = exp(log_c_1);
   c_2 = exp(log_c_2);

// the cov effects need seperate loop because number of covariates varies between lifestage (currently both 1 - eventually will vary)
  for(t in 1:nByrs){
   for (c in 1:ncovars1) {
  cov_eff1[t+1,c] = theta1[c]*cov1[t+1,c]; // covariates for juveniles t+1 
   }
   for (c in 1:ncovars2) {
     if(c==1){ // GOA temperature  
  cov_eff2[t+2,c] = theta2[c]*cov2[t+2,c]; // first winter, t+a+1, a=1 
     }
     if(c==2){ // Chum hatchery - shouldn't be forwarding looking to compare the covariate, because they are hatchery releases, so releases during the brood year, will be competing in the ocean at t+2
  cov_eff2[t+2,c] = theta2[c]*cov2[t,c]; //  
     }
     
   }
    p_1[t+1]  = 1 / (1 + exp(basal_p_1+ sum(cov_eff1[t+1,1:ncovars1])));
    p_2[t+2]  = 1 / (1 + exp(basal_p_2+ sum(cov_eff2[t+2,1:ncovars2])));
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

   for (t in 1:nByrs){ // loop for each brood year 
         
         N_e_sum[t] = sum(N_e[t,1:A]); 
         
         kappa_j_survival[t+1] =  p_1[t+1]/(1 + (p_1[t+1]*N_e_sum[t])/c_1); // Eq 4.1  - Bev holt transition estimating survival from Egg to Juvenile (plugs into Eq 4.4) 

         N_j[t+1] = kappa_j_survival[t+1]*N_e_sum[t]; // Eq 4.4  generated estimate for the amount of fish each year and stock that survive to a juvenile stage
        
         kappa_marine_survival[t+2] =  p_2[t+2]/(1 + (p_2[t+2]*N_j[t+1])/c_2); //Eq 4.1  - Bev holt transition estimating survival from juvenile to spawner (plugs into Eq 4.4) 

    // convert survival to mortality for next equation
         kappa_marine_mortality[t+2] = -log(kappa_marine_survival[t+2]);
     
        for (a in 1:A) { 
          N_first_winter[t+a+2,a] =  N_j[t+1]*p[a]; // add age structure, p is proportion per age class
       
          N_recruit[t+a+2,a] = N_first_winter[t+a+2,a]*exp(-(sum(M[1:a]) + kappa_marine_mortality[t+2])); // add age specific mortality, 

          N_catch[t+a+2,a] = N_recruit[t+a+2,a]*(1-exp(-F[t+a+2]));
           
          N_sp[t+a+2,a] = N_recruit[t+a+2,a]-N_catch[t+a+2,a]; // fishing occurs before spawning -- 
             
          N_e[t+a+2,a] = fs[a]*Ps*N_sp[t+a+2,a]; 
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
  // sigma_y_j ~ uniform(0,5); //normal 
 
  log_catch_q ~ normal(0,5);//normal(-1.2,4); // Estimate Q - this will translate # of recruits to # of spawners 
  
  log_c_1 ~ normal(16, 10); // carrying capacity prior - stage 1 20
  log_c_2 ~ normal(20, 10); // carrying capacity prior - stage 2 16

  // log_c_1 ~ normal(16, 5); // carrying capacity prior - stage 1
  // log_c_2 ~ normal(14, 5); // carrying capacity prior - stage 2

  N_j_start_log ~ normal(13.6,5); //1

 for(t in 1:t_start){
    N_first_winter_start_log[t] ~ normal(13.57,5);//1
    N_sp_start_log[t] ~ normal(13.48,5);//2
    N_recruit_start_log[t] ~  normal(13.5,5);//2
    N_catch_start_log[t] ~ normal(12.3,5);//1
    N_egg_start_log[t] ~  normal(14.5, 5);//2
//    N_egg_start_log[t] ~  normal(13.7, 0.1); // starting value for eggs, initiates pop model
}


  // print("N_e_sum_start_log:", N_e_sum_start_log);
      // print("N_catch_start_log:", N_catch_start_log);
      // print("N_sp_start_log:", N_sp_start_log);
 theta1[1] ~ normal(0,0.01); //normal(0.5,5); // environmental covariate coefficient stage 1
 theta1[2] ~ normal(0,0.01); // environmental covariate coefficient stage 1
 // theta1[3] ~ normal(0,0.01); 
 
 // theta1[3]  ~ normal(0,0.01); //normal(0.5,5); // environmental covariate coefficient stage 1
 // theta1[4] ~ normal(0,0.01); // environmental covariate coefficient stage 1

 theta2[1] ~ normal(0,0.01);
 theta2[2] ~ normal(0,0.01);
    // theta2[2] ~ normal(0,1); // environmental covariate coefficient stage 1
    // theta2[3]  ~ normal(0,1); //normal(0.5,5); // environmental covariate coefficient stage 1
    // theta2[4] ~ normal(0,1);
 
    D_scale ~ beta(1,1);  
    
    basal_p_1 ~ beta(1,1); // mean survival stage 1
    basal_p_2 ~ beta(1,1); // mean survivial stage 2
  
// age comp 
    for (a in 1:A) {
   target += gamma_lpdf(g[a]|Dir_alpha[a],1);
 }
 
// log fishing mortality for each calendar year 
    log_F_mean ~ normal(0,1);
 
  for(t in 1:nRyrs_T){
    log_F_dev_y[t] ~ normal(0, 5); 
 }

 // age comp priors -- maturity schedules
  prob[1] ~ beta(1,1); 
  prob[2] ~ beta(1,1);  
  prob[3] ~ beta(1,1);  
 
// Likelilihoods --  
  // Observation model
  for (t in 1:nByrs) {
     target += normal_lpdf(log(data_stage_j[t]) | log(N_j_predicted[t]), sqrt(log((0.05^2) + 1))); //sigma_y_j); // not sure if this is liklihood is right, returning here is escapement + harvest
    } 

// // Likelilihoods --  
//   // Observation model
//   for (t in 1:nByrs) {
//      log(data_stage_j[t]) ~ normal(log(N_j_predicted[t]), sigma_y_j);////sqrt(log((0.06^2) + 1))); // sqrt(log((data_j_cv[t]) + 1)));//sigma_y_j);  
//     } 

  for(t in 1:nRyrs){ // calendar years 

     target += ess_age_comp[t]*sum(o_run_comp[t,1:A] .* log(q[t,1:A])); // ESS_AGE_COMP right now is fixed
     
     target += normal_lpdf(log(data_stage_return[t]) | log(sum(N_recruit[t,1:A])),sqrt(log((0.05^2) + 1))); //sqrt(log((data_recruit_cv[t]^2) + 1))); // sqrt(log((0.06^2) + 1))); // not sure if this is liklihood is right, returning here is escapement + harvest
     target += normal_lpdf(log(data_stage_harvest[t]) | log(sum(N_catch[t,1:A])), sqrt(log((0.01^2) + 1)));    
     target += normal_lpdf(log(data_stage_sp[t]) |  log(sum(N_sp[t,1:A])), sqrt(log((0.05^2) + 1)));//sqrt(log((data_sp_cv[t]^2) + 1))); // sigma_y_sp);

  }
}  

generated quantities{
real  theta_1_1_pp ;   
real  theta_1_2_pp ; 
// real  theta_1_3_pp ; 

real  theta_2_1_pp ;
real  theta_2_2_pp ;

real N_sp_pp [nRyrs];
real N_j_pp [nByrs];

// added log normal correcrtions 
theta_1_1_pp = normal_rng(theta1[1]- 0.5 * 0.25^2,0.25);
theta_1_2_pp = normal_rng(theta1[2]- 0.5 * 0.25^2,0.25);
// theta_1_3_pp = normal_rng(theta1[3]- 0.5 * 0.25^2,0.25);
 
theta_2_1_pp = normal_rng(theta2[1]- 0.5 * 0.25^2,0.25);
theta_2_2_pp = normal_rng(theta2[2]- 0.5 * 0.25^2,0.25);

for(t in 1:nRyrs){
N_sp_pp[t] = normal_rng(log(sum(N_sp[t,1:A]))- 0.5 * sqrt(log((data_sp_cv[t]^2) + 1))^2, sqrt(log((data_sp_cv[t]^2) + 1))); 
  }
  
for(t in 1:nByrs){
 N_j_pp[t] = normal_rng(log(N_j_predicted[t])- 0.5 * 0.05^2, 0.05); // sigma_y_j^2, sigma_y_j);
  }
}

