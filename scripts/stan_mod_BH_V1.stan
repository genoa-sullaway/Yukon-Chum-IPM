data {
  int<lower=0> N ; // total observations 
  int<lower=0> K; // number of stocks
  
  // stock specific data 
  int<lower=0> N_stock[K];  // Number of data points for each stock
  vector[N_stock[K]] ssb[K];  // Spawners for each stock
  vector[N_stock[K]] rec[K];  // Recruitment for each stock
  }
// transformed data { my code 
// vector[N_stock[K]] log_rec[K]; // log recruitment
// log_rec[K] = log(rec[K]); // transform recruitment to log space
// }
transformed parameters { // chat gpt code
vector[K] beta; 
vector[N_stock[K]] log_rec[K]; // log recruitment

  for (k in 1:K) {
    log_rec[k] = log(rec[k]);
    beta[k] = exp(log_beta[k]);
  }
}
parameters {
  real<lower=0> sigma_y;
  // Stock specific parameters
  real<lower = 0> alpha[K]; // max recruitment
  real log_beta[K]; 
  
  // Population-level parameters -- from Chatgpt, do i need/want population level?? 
  // real<lower=0> mu_alpha;
  // real<lower=0> mu_beta;
  // real<lower=0> sigma_alpha;
  // real<lower=0> sigma_beta;

} 
transformed parameters {

real beta[K] = exp(log_beta[K]); // Beta for each stock

vector[N_stock[K]] rhat[K]; // predicted recruitment for each stock

vector[N_stock[K]] log_rhat[K]; // predicted log recruitment for each stock
 
for (k in 1:K) {
rhat[k] =  (ssb[k] * alpha[k]) ./ (1 + (beta[k] * ssb[k])); // beverton holt model
}
log_rhat[K] = log(rhat[K]);

}

model {
  //priors
  sigma_y ~ cauchy(0, 2.5);
  
for(k in 1:K) {
  log_rec[k] ~ normal(log_rhat[k], sigma_y); //account for retransformation bias, recruits
  alpha[k] ~ normal(10,10); // Stan specific loop to assign priors across stocks
  log_beta[k] ~ normal(1,5);
  }
}
generated quantities {
vector[N_stock[K]] pp_rhat[K];

  for (k in 1:K) {

   pp_rhat[k] = exp(normal_rng(log_rhat[k] - 0.5 * sigma_y^2, sigma_y)); // generate posterior predictives with backtransform? 

  }
}

