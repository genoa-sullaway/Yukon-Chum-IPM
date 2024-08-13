data { // all equation references are from proposal numbering
  int<lower=0> nByrs ; // number of brood years  
  int<lower=0> nRyrs;  // Number of recruitment years in liklihood
  int<lower=0> nRyrs_T;  // Number of TOTAL recruitment years in SR model  
  int<lower=0> A;     // Number of age classes - 4
  int<lower=0> t_start;   // Number of age classes x2 for filling in starting values  
  
  real<lower=0> Ps; // Proportion of females in spawning stock, based on lit - currently 50%
  vector [A] fs; // fecundity
  vector [A] M; // fixed mortality for 3 older age classes

  vector[nByrs] data_stage_j;    // number of juveniles for each group  (basis)
  vector[nByrs] data_stage_return;   //  number of harvest + escapement for each group 
  vector[nRyrs] data_stage_sp;   // number of spawners for each group (escapement)
  vector[nRyrs] data_stage_harvest;   // number of spawners for each group (escapement)

  real  N_j_start_log;
  real  N_brood_year_return_start_log;
  real  N_sp_start_log[t_start,A];
  real  N_recruit_start_log[t_start,A];
  real  N_catch_start_log[t_start,A];
  real  N_egg_start_log[t_start,A];
//   
// vector<lower=0, upper=1> [A] pi; // actual age comps
//  
int<lower=0> ncovars1; //number of covariates for first lifestage  
int<lower=0> ncovars2; //number of covariates for second lifestage  

matrix [nByrs, ncovars1] cov1; // covariate data in a matrix format
matrix [nByrs, ncovars2] cov2; // covariate data in a matrix format
 
matrix<lower=0, upper=1>[nRyrs,A] o_run_comp; // Observed age composition by year
// vector [nByrs] ess_age_comp;   // Effective input sample size for age comp "observations" -  currently fixed to 200 based on Hulson et al 2011
real ess_age_comp; 
}
  transformed data {
real N_sp_start [t_start,A];
real N_recruit_start [t_start,A];
real N_catch_start [t_start,A];
real N_ocean_start[t_start,A];
real N_egg_start[t_start,A];
real N_j_start;
real N_brood_year_return_start;

for(t in 1:t_start){
  for(a in 1:A){
  N_sp_start[t,a] = exp(N_sp_start_log[t,a]);//*p[a]; //o_run_comp[t,a];
  N_recruit_start[t,a] = exp(N_recruit_start_log[t,a]);//*p[a]; //*o_run_comp[t,a];
  N_catch_start[t,a] = exp(N_catch_start_log[t,a]);//*p[a];//o_run_comp[t,a];
  N_egg_start[t,a] = exp(N_egg_start_log[t,a]);//*p[a];//o_run_comp[t,a];
  }
 }

N_j_start = exp(N_j_start_log);
N_brood_year_return_start = exp(N_brood_year_return_start_log);

}  
parameters {
 real  log_c_1;
 real  log_c_2; // log carrying capacity
 real <lower=0> sigma_catch; 
  // real <lower=0> sigma_y_j;
// 
// // starting values 
//  real <lower= 10>  N_j_start_log; 
//  real <lower =10>  N_brood_year_return_start_log; 
//  real <lower= 10>  N_sp_start_log [t_start,A];
//  real <lower= 10>  N_recruit_start_log [t_start,A];
//  real <lower= 10>  N_catch_start_log [t_start,A];
//  real <lower= 10>  N_egg_start_log [t_start,A]; 

// covariate parameters 
real theta1 [ncovars1]; // covariate estimated for each covariate and each population
real theta2 [ncovars2];

vector <lower=0> [A-1] prob; 
// real <lower=0, upper=1> D_scale;     // Variability of age proportion vectors across cohorts
// real <lower=0> g[nByrs,A]; // gamma random draws
real log_catch_q;
 
// vector [nRyrs_T]  log_F_dev_y; 
// real log_F_mean; 

 vector [nRyrs_T]  log_F; 

real <lower=0, upper = 1> basal_p_1; // mean alpha for covariate survival stage 1
real <lower=0, upper = 1> basal_p_2; // mean alpha for covariate survival stage 2
 }

transformed parameters { 
 vector [nByrs] N_j; // predicted juveniles 
 vector [nByrs] N_j_predicted; // predicted juveniles this goes into the liklihood- gets transformed by estimates of Q
 vector [nByrs] N_e_sum; // sum eggs across ages to then go into the lifecycle section that doesnt use age 
vector [nByrs] N_brood_year_return; // sum eggs across ages to then go into the lifecycle section that doesnt use age 

 real N_recruit [nRyrs_T,A]; 
 real N_sp [nRyrs_T,A];
 real N_catch [nRyrs_T,A];
 real N_ocean[nRyrs_T,A];
 real N_e [nRyrs_T,A];

// real N_sp_start [t_start,A];
// real N_recruit_start [t_start,A];
// real N_catch_start [t_start,A];
// real N_egg_start[t_start,A];
// real N_brood_year_return_start;
// // real N_first_winter_start[t_start,A];
// real N_j_start;

real c_1;
real c_2;

vector<lower = 0> [nRyrs_T] F;

// survival and covariate section 
vector  <lower=0,upper = 1> [nByrs] p_1; // productivity in bev holt transition funciton, 1 = FW early marine
vector <lower=0,upper = 1>  [nByrs] p_2;

// vector <lower=0.001, upper = 0.99> [nByrs+1] p_1; // productivity in bev holt transition funciton, 1 = FW early marine
// vector <lower=0.001, upper = 0.99> [nByrs+1+1] p_2;

vector<lower=0,upper = 1> [nByrs] kappa_j_survival; // predicted survival for juvenile fish (FW and early marine)
//vector <lower=0.001, upper = 0.5> [nByrs] kappa_j_survival; // predicted survival for juvenile fish (FW and early marine)
vector<lower=0,upper = 1> [nByrs] kappa_marine_survival; // predicted survival for marine fish
// vector [nByrs] kappa_marine_mortality; // converting kappa marine survival to mortality 

matrix [nByrs, ncovars1] cov_eff1; // array that holds FW and early marine covariate effects by brood year and stock
matrix [nByrs, ncovars2] cov_eff2; 

real <lower=0>  catch_q; // related juvebile data to spawner data (on different scales) gets transfomed from log to number 
 
// Age related transformed params ====== 
// matrix<lower=0, upper=1> [nByrs,A] p;// proportion of fish from each brood year that mature at a certain age
// real<lower=0> D_sum;                   // Inverse of D_scale which governs variability of age proportion vectors across cohorts
// vector<lower=0> [A] Dir_alpha;          // Dirichlet shape parameter for gamma distribution used to generate vector of age-at-maturity proportions
matrix <lower=0, upper=1>[nRyrs,A] q; 
vector<lower=0, upper=1> [A] pi; // actual age comps
  
// starting value transformations ======
  // kappa_marine_survival[1:2] = kappa_marine_start;
  // kappa_marine_mortality[1:2] = kappa_marine_mort_start;
  // kappa_j_survival[1]= kappa_j_start;
 
 //  for(t in 1:nRyrs_T){//  
 //  // instant fishing mortality 
 //  F[t]  = exp(log_F_mean +log_F_dev_y[t]);
 // }
 
  for(t in 1:nRyrs_T){//
  // instant fishing mortality
   F[t] = exp(log_F[t]); 
  // F[t]  = exp(log_F_mean +log_F_dev_y[t]);
 }
 
  // N_j_start = exp(N_j_start_log); 
  // N_brood_year_return_start = exp(N_brood_year_return_start_log);
  // 
// for(t in 1:t_start){
//   for(a in 1:A){
//   N_sp_start[t,a] = exp(N_sp_start_log[t,a]); 
//   N_recruit_start[t,a] = exp(N_recruit_start_log[t,a]); 
//   N_catch_start[t,a] = exp(N_catch_start_log[t,a]); 
//   N_egg_start[t,a] = exp(N_egg_start_log[t,a]); 
//   }
//  }

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

  
  N_j[1] = N_j_start;
  N_brood_year_return[1] = N_brood_year_return_start;
 
    for(a in 1:A){
   // add starting values to the whole population array
  N_recruit[1:t_start,a] = N_recruit_start[1:t_start,a];
  N_sp[1:t_start,a] = N_sp_start[1:t_start,a];
  N_catch[1:t_start,a] = N_catch_start[1:t_start,a];
  N_e[1:t_start,a] = N_egg_start[1:t_start,a]; 
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
  cov_eff1[t,c] =  theta1[c]*cov1[t,c]; // covariates for juveniles t+1 
   }
   for (c in 1:ncovars2) {
      // if(c==1){ // GOA temperature  
  cov_eff2[t,c] =  theta2[c]*cov2[t,c]; // first winter, t+a+1, a=1 
     // }
  //    if(c==2){ // pink hatchery - shouldn't be forwarding looking to compare the covariate, because they are hatchery releases, so releases during the brood year, will be competing in the ocean at t+2
  // cov_eff2[t,c] =  theta2[c]*cov2[t,c]; //  
     }
  //    if(c==2){ // Chum hatchery - shouldn't be forwarding looking to compare the covariate, because they are hatchery releases, so releases during the brood year, will be competing in the ocean at t+2
  // cov_eff2[t+2,c] = theta2[c]*cov2[t,c]; //  
  //    }
    p_1[t]  = 1 / (1 + exp(-basal_p_1-sum(cov_eff1[t,1:ncovars1])));
    p_2[t]  = 1 / (1 + exp(-basal_p_2- sum(cov_eff2[t,1:ncovars2])));
  }

 // Maturity schedule: use a common maturation schedule to draw the brood year specific schedules
  pi[1] = (prob[1]);
  pi[2] = (prob[2]) * (1 - pi[1]);
  pi[3] = (prob[3]) * (1 - pi[1] - pi[2]);
  pi[4] = 1 - (pi[1] - pi[2] - pi[3]);

  // D_sum = 1/(D_scale^2);
  // 
  // for (a in 1:A) {
  //   Dir_alpha[a] = D_sum * pi[a];
  // for(t in 1:(nByrs)) {
  //   p[t,a] = g[t,a]/sum(g[t,1:A]);
  //  }
  // }
 
catch_q = exp(log_catch_q); // Q to relate basis data to recruit/escapement data -- Is this right??

     for (t in 1:nByrs){ // loop for each brood year 
         N_e_sum[t] = sum(N_e[t,1:A]);
                  
         kappa_j_survival[t] =  p_1[t]/(1 + ((p_1[t]*N_e_sum[t])/c_1)); // Eq 4.1  - Bev holt transition estimating survival from Egg to Juvenile (plugs into Eq 4.4) 

         N_j[t] = kappa_j_survival[t]*N_e_sum[t]; // Eq 4.4  generated estimate for the amount of fish each year and stock that survive to a juvenile stage
     
         kappa_marine_survival[t] =  p_2[t]/(1 + ((p_2[t]*N_j[t])/c_2)); //Eq 4.1  - Bev holt transition estimating survival from juvenile to spawner (plugs into Eq 4.4) 

         // convert survival to mortality for next equation
         // kappa_marine_mortality[t] = -log(kappa_marine_survival[t]);
      
           // N_first_winter[t] = N_j[t]*kappa_marine_survival[t]; //)*exp(-(kappa_marine_mortality[t])) #add age specific mortality, 
           N_brood_year_return[t] = N_j[t]*kappa_marine_survival[t]; //)*exp(-(kappa_marine_mortality[t])) #add age specific mortality, 
         
        for (a in 1:A) { 
           N_recruit[t+a+2,a] = (N_brood_year_return[t]*pi[a]); //*exp(-(sum(M[1:a]))); //exp(-(kappa_marine_mortality[t])) #add age specific mortality, 
           
          // N_first_winter[t+a+1,a] =  N_j[t]*p[t+a+1,a]; // add age structure, p is proportion per age class

          // N_recruit[t+a+1,a] = N_first_winter[t+a+1,a]*exp(-(sum(M[1:a]) + kappa_marine_mortality[t])); // add age specific mortality,
          
           // N_recruit[t+a+1,a] = (N_j[t]*p[t,a])*exp(-(sum(M[1:a]) + kappa_marine_mortality[t])); // add age specific mortality, 
           // N_recruit[t+a,a] = (N_j[t]*p[t,a])*exp(-(kappa_marine_mortality[t])); // add age specific mortality, 
          
          N_catch[t+a+2,a] = N_recruit[t+a+2,a]*(1-exp(-(F[t+a+2])));
         // N_catch[t+a,a] = N_recruit[t+a,a]*(1-exp(-(F[t+a]*S[a])));
           
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
  // sigma_y_j ~ normal(0,5); //normal 
   sigma_catch ~ normal(0,10); 
   
   log_catch_q ~ normal(-4,10);//normal(-1.2,4); // Estimate Q - this will translate # of recruits to # of spawners 

  log_c_1 ~  normal(16, 50); // carrying capacity prior - stage 1
  log_c_2 ~  normal(18, 50); // carrying capacity prior - stage 2
// 
//  N_j_start_log ~ normal(17,5);
//  N_brood_year_return_start_log~ normal(16,5); 
// 
//  for(t in 1:t_start){
//    for(a in 1:A){
//     N_sp_start_log[t,a] ~ normal(15,5);
//     N_recruit_start_log[t,a] ~  normal(15.5,5);
//     N_catch_start_log[t,a] ~ normal(13,5);
//     N_egg_start_log[t,a] ~  normal(18,5);
//   }
 
    // print("g:", g);
    // print("prob :", prob);
    
  theta1[1] ~ normal(0,1); //normal(0.5,5); // environmental covariate coefficient stage 1
 // theta1[2] ~ normal(0.1,0.01); // environmental covariate coefficient stage 1
 // theta1[3] ~ normal(-0.1,0.01);
 
 theta2[1] ~ normal(0,1);
 // theta2[2] ~ normal(-0.1,0.01);
 
    basal_p_1 ~ beta(1,1); // mean survival stage 1
    basal_p_2 ~ beta(1,1); // mean survivial stage 2
  
// age comp 
//  for(t in 1:nByrs){
//     for (a in 1:A) {
//    target += gamma_lpdf(g[t,a]|Dir_alpha[a],1);
//  }
// }

// log fishing mortality for each calendar year 
 //    log_F_mean ~ normal(0,1);
 // 
 //  for(t in 1:nRyrs_T){
 //    log_F_dev_y[t] ~ normal(0, 1); 
 // }
 
   for(t in 1:nRyrs_T){
 log_F[t] ~ normal(0,1); //log fishing mortatliy
}


 // age comp priors -- maturity schedules
  prob[1] ~ beta(1,1);
  prob[2] ~ beta(1,1);
  prob[3] ~ beta(1,1);
  
 for (t in 1:nByrs) {
     target += normal_lpdf(log(data_stage_j[t]) | log(N_j_predicted[t]), sqrt(log((0.01^2) + 1)));  
 // recruit by brood year 
     target += normal_lpdf(log(data_stage_return[t]) | log(N_brood_year_return[t]), sqrt(log((0.01^2) + 1)));  
     
    } 

  for(t in 1:nRyrs){ // calendar years 
 // if(t<nByrs){
     target += ess_age_comp*sum(o_run_comp[t,1:A] .* log(q[t,1:A])); // ESS_AGE_COMP right now is fixed
     
     //recruit by cal year:: target += normal_lpdf(log(data_stage_return[t]) | log(sum(N_recruit[t,1:A])), sqrt(log((0.06^2) + 1)));  
     target += normal_lpdf(log(data_stage_harvest[t]) | log(sum(N_catch[t,1:A])), sigma_catch) ; //sqrt(log((0.01^2) + 1)));  
     target += normal_lpdf(log(data_stage_sp[t]) |  log(sum(N_sp[t,1:A])), sqrt(log((0.01^2) + 1))); //sqrt(log((data_sp_cv[t]) + 1))); // sigma_y_sp);
      // }
    }
  }
  


// generated quantities{
// real  theta_1_1_pp ;   
// real  theta_1_2_pp ; 
// real  theta_1_3_pp ; 
// 
// real  theta_2_1_pp ;
// real  theta_2_2_pp ;
// 
// // real log_N_sp_pp [nRyrs];
// real N_sp_pp [nRyrs];
// real N_catch_pp [nRyrs];
// real N_rec_pp [nRyrs];
// real N_j_pp [nByrs];
// 
// // added log normal correcrtions 
// theta_1_1_pp = normal_rng(theta1[1]- 0.5 * 0.01^2,0.05);
// theta_1_2_pp = normal_rng(theta1[2]- 0.5 * 0.01^2,0.05);
// theta_1_3_pp = normal_rng(theta1[3]- 0.5 * 0.01^2,0.05);
//  
// theta_2_1_pp = normal_rng(theta2[1]- 0.5 * 0.01^2,0.05);
// theta_2_2_pp = normal_rng(theta2[2]- 0.5 * 0.01^2,0.05);
// 
// for(t in 1:nRyrs){
// // log_N_sp_pp[t] = lognormal_rng(log(sum(N_sp[t,1:A]))), 1); //(sqrt(log(data_sp_cv[t]^2) + 1))); 
// N_sp_pp[t] = normal_rng((sum(N_sp[t,1:A]))- 0.5 * sqrt((data_sp_cv[t]^2) + 1)^2, 1); //(sqrt(log(data_sp_cv[t]^2) + 1))); 
// N_catch_pp[t] = normal_rng((sum(N_catch[t,1:A]))- 0.5 * sqrt((0.1^2) + 1)^2, 1);//(0.1)); 
// N_rec_pp[t] = normal_rng((sum(N_recruit[t,1:A]))- 0.5 * sqrt((data_recruit_cv[t]^2) + 1)^2, 1);//(sqrt(log(data_recruit_cv[t]^2) + 1))); 
// 
//   }
//   
// for(t in 1:nByrs){
//  N_j_pp[t] = normal_rng((N_j_predicted[t])- 0.5 *0.5^2, 0.5); //0.05^2, 0.05); // 
//   }
// }

