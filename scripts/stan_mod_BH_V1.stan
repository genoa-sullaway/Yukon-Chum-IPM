data {
  int<lower=0> N ;
  int ssb[N] ; 
  int rec[N] ;
}
parameters {
  real<lower=0> sigma_y;
  real alpha;
  real beta;
}
// transformed parameters {
//   real alpha;
//   real beta;
//   alpha = exp(log_alpha);
//   beta = exp(log_beta);
// } 

model {
  sigma_y ~ cauchy(0, 5);
  alpha ~ uniform(0,10);
  beta ~ uniform(0,10);
 
  for (i in 1:N){
    rec[i] ~ normal((ssb[i] * log(alpha)) / (1+ beta*ssb[i]) ,sigma_y);
  }
}

generated quantities {
  vector[N] log_lik;
  for (i in 1:N){
    log_lik[i] = normal_lpdf(rec[i] | (ssb[i] * alpha) / (1 + beta*ssb[i]), sigma_y);
  }
}
