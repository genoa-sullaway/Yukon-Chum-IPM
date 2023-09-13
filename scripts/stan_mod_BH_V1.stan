data {
  int<lower=0> N ; // total observations 
  int<lower=0> K; // number of stocks
  
  // stock specific data 
  int<lower=0> N_stock[K];  // Number of data points for each stock
  vector[N] ssb; // Spawners for each stock, stacked
  vector[N] rec; // recruits for each stock, stacked. 
  int<lower = 1, upper = K> g[N]; // Vector of group assignments so STAN knows which group it is dealing with in the set of N observations 
  }
  transformed data {
  vector[N] log_rec; // log recruitment
// log_rec = log(rec); // transform recruitment to log space
 
 for (k in 1:N) {
    log_rec[k] = log(rec[k]); // recruitment data
 }
}
parameters {
  real<lower=0> sigma_y ; // error for each stock
  // Stock specific parameters
  real<lower = 0> alpha_a[K]; // productivity for each stock
  real log_beta_a[K]; // log beta for each stock
   real<lower = 0> alpha_b[K]; // productivity for each stock
  real log_beta_b[K]; // log beta for each stock
  
  // Population-level parameters -- from Chatgpt, do i need/want population level?? 
  // real<lower=0> mu_alpha;
  // real<lower=0> mu_beta;
  // real<lower=0> sigma_alpha;
  // real<lower=0> sigma_beta;
} 
transformed parameters { 
real rhat[N]; // predicted recruitment for each stock
real log_rhat[N]; // predicted log recruitment for each stock
real beta[K];

  for (k in 1:K){
    beta[k] = exp(log_beta[k]); // Beta for each stock
  }
  
 
  for (y in 1:N) {
    rhat[y] =  (ssb[y] * alpha[g[y]]) ./ (1 + (beta[g[y]] * ssb[y])); // beverton holt model
  }
 

for (y in 1:N) {
  log_rhat[y] = log(rhat[y]); // predicted recruitment
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
  alpha[k] ~ normal(10,10); // Stan specific loop to assign priors across stocks
  log_beta[k] ~ normal(1,5);
}
  
 
  for (i in 1:N) {
   log_rec[i] ~ normal(log_rhat[i], sigma_y); //account for retransformation bias, recruits
  }
 
  
}
generated quantities {
	real<lower=0> pp_rhat[N]; // predicted recruits 

 
  for (i in 1:N) {
   pp_rhat[i] = exp(normal_rng(log_rhat[i] - 0.5 * sigma_y^2, sigma_y)); // generate posterior predictives with backtransform? 
    }
  }
 

