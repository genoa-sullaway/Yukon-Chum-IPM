// old stan model on log scale 

data {
  int<lower=0> N ; // observations 
  vector[N] ssb ; 
  vector[N] rec ;
}
transformed data {
vector[N] log_rec; // log recruitment
log_rec = log(rec); // transform recruitment to log space
}
parameters {
  real<lower=0> sigma_y;
  real<lower = 0> alpha; // max recruitment
  // real log_alpha;
  real log_beta;
}
// transformed parameters {
//   real alpha;
//   real beta;
//   alpha = exp(log_alpha);
//   beta = exp(log_beta);
// } 
transformed parameters {

real beta = exp(log_beta);

vector[N] rhat;

vector[N] log_rhat;

rhat =  (ssb * alpha) ./ (1 + (beta * ssb)); // beverton holt model

log_rhat = log(rhat);

}

model {
  log_rec ~ normal(log_rhat, sigma_y); //account for retransformation bias

  sigma_y ~ cauchy(0, 2.5);
  alpha ~ normal(10,10);
  log_beta ~ normal(1,5);
 
}
generated quantities {
 vector[N] pp_rhat;

  for (i in 1:N) {

   pp_rhat[i] = exp(normal_rng(log_rhat[i] - 0.5 * sigma_y^2, sigma_y)); // generate posterior predictives with backtransform? 

  }
}

