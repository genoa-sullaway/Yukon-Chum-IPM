data {
  int<lower=0> N ; // total observations 
  int<lower=0> K; // number of stocks
  
  // stock specific data 
  int<lower=0> N_stock[K];  // Number of data points for each stock
  vector[N] stage_a; 
  vector[N] stage_b;   
  vector[N] stage_c; 
  int<lower = 1, upper = K> g[N]; // Vector of group assignments so STAN knows which group it is dealing with in the set of N observations 
  }
  transformed data {
  vector[N] log_stage_b; // log stage b (old recruitment..)
  vector[N] log_stage_c; // log stage b (old recruitment..)
// log_rec = log(rec); // transform recruitment to log space
 
 for (k in 1:N) {
    log_stage_b[k] = log(stage_b[k]); //  
    log_stage_c[k] = log(stage_c[k]); //  
 }
}
parameters {
  real<lower=0> sigma_y ; // error for each stock
  // Stock specific parameters
  real<lower = 0> alpha_1[K]; // productivity for each stock
  real log_beta_1[K]; // log beta for each stock
  real<lower = 0> alpha_2[K]; // productivity for each stock
  real log_beta_2[K]; // log beta for each stock
  
  // Population-level parameters -- from Chatgpt, do i need/want population level?? 
  // real<lower=0> mu_alpha;
  // real<lower=0> mu_beta;
  // real<lower=0> sigma_alpha;
  // real<lower=0> sigma_beta;
} 
transformed parameters { 
real rhat_b[N]; // predicted recruitment for each stock
real log_rhat_b[N]; // predicted log recruitment for each stock
real rhat_c[N]; // predicted recruitment for each stock
real log_rhat_c[N]; // predicted log recruitment for each stock
real beta_1[K];
real beta_2[K];

  for (k in 1:K){
    beta_1[k] = exp(log_beta_1[k]); // Beta for each stock
    beta_2[k] = exp(log_beta_2[k]); // Beta for each stock
  }
  
  for (y in 1:N) {
    rhat_b[y] =  (stage_a[y] * alpha_1[g[y]]) ./ (1 + (beta_1[g[y]] * stage_a[y])); // beverton holt transition model a 
    rhat_c[y] =  (stage_b[y] * alpha_2[g[y]]) ./ (1 + (beta_2[g[y]] * stage_b[y])); // beverton holt transition model b 
  }
 
for (y in 1:N) {
  log_rhat_b[y] = log(rhat_b[y]); // predicted recruitment
   log_rhat_c[y] = log(rhat_c[y]); // predicted recruitment
  }
}

model {
  //priors
  //sigma_y ~ cauchy(0, 2.5);
  
  // for (k in 1:N) {
  //   // Likelihood for log_rec[k] with adjustment for the transformation
  //   target += -0.5 * ((log_rec[k] - log_rhat[k]) / sigma_y)^2;
  // }
   sigma_y ~ cauchy(0, 2.5);
   
for(k in 1:K) {
  alpha_1[k] ~ normal(10,10); // Stan specific loop to assign priors across stocks
  log_beta_1[k] ~ normal(1,5);
  alpha_2[k] ~ normal(10,10); // Stan specific loop to assign priors across stocks
  log_beta_2[k] ~ normal(1,5);
}
  
 
  for (i in 1:N) {
   log_stage_b[i] ~ normal(log_rhat_b[i], sigma_y); //account for retransformation bias, recruits
   log_stage_c[i] ~ normal(log_rhat_c[i], sigma_y); //account for retransformation bias, recruits
  }
 
  
}
generated quantities {
	real<lower=0> pp_rhat_b[N]; // predicted age ocean 0 "recruits"
	real<lower=0> pp_rhat_c[N]; // predicted returning adult fish 
 
  for (i in 1:N) {
   pp_rhat_b[i] = exp(normal_rng(log_rhat_b[i] - 0.5 * sigma_y^2, sigma_y)); // generate posterior predictives with backtransform? 
   pp_rhat_c[i] = exp(normal_rng(log_rhat_c[i] - 0.5 * sigma_y^2, sigma_y)); // generate posterior predictives with backtransform? 
    }
  }
 

