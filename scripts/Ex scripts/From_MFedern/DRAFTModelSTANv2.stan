//Ricker Model For Calculating Pacific Salmon Productivity
//Notes:
  //  matrix[3, 3] m[6, 7] - m to be a two-dimensional array of size 6 x 7,
//    containing values that are 3 x 3 matrices. 

data {
  int<lower=0> NS; //number of stocks
  int<lower=0> N;  //total observations across all stocks
  int<lower=0> S[N]; //stock pointer vector for each stocl
  int<lower=0> ncovars; //number of covariates
  //vector[NS] ny;  //number of years of SR observations for each stock
  real ln_rec[N]; //recruitment data
  real error_rec[N]; //observed recruitment error
  real error_spawn[N]; // observed spawner error
  real spawn[N]; // observed spawners
 // real ln_spawn[N]; // observed spawners
  real covars[N, ncovars]; // covariate data note: cannot be coded as matrix 
}

parameters {
  // PARAMETERS
  // Ricker Params
  //real<lower=0> mu_s[1]; //overall mean across populations
 // real<lower=0> mu[1];
  real<lower=0, upper = 5> mu_alpha[1]; //group level mean across populations
  real<lower=0> sigma_alpha[1]; // group level SD across populations
 // real<lower=1, upper = 2> mu_sigma_oe[1]; //group level mean across populations
 // real<lower=0, upper = 0.5> sigma_sigma_oe[1]; // group level SD across populations
  real<lower=0> alpha[NS]; // alpha is max productivity
  real<lower=0> beta[NS]; // beta is equilibrium abundance
 // real<lower=0> spawn_mu[NS]; // mean spawning abundance by stock
  real<lower=0> spawn_pred[N]; // spawner predicted incorporates spawn and spanw error data from the likelihood 
 // Process Error
  //real<lower=1, upper = 2> sigma_oe[NS];
  real<lower=0> sigma_oe[NS];
    //real<lower=0> sigma_oe[N];

// real<lower=0> phi[NS]; //group level mean across populations
 //COEFFICIENT
 real theta[NS, ncovars]; // covariate estimated for each covariate and each population 
  real mu_coef[ncovars]; // mean covariate effect across populations (group level hierarchical effect)
   real<lower=0> sigma_coef[ncovars]; // error of the covariate effect across populations (group level hierarchical effect)
}

transformed parameters {
	//PREDICTIONS
	 real cov_eff[N, ncovars]; // overall covariate effect (sum of all covariates for one population)
	real<lower=0> pred[N]; // predicted recruits 
	//real ln_pred[N]; // predicted recruits 
			 for(y in 1:N){
			 	for(c in 1:ncovars){
			  cov_eff[y,c]= theta[S[y],c]*covars[y, c]; // S[y] is the population pointer vector so theta is estimated for each population for each covariate, not every year. See ragged and missing data structure in stan manual for structure
	 }// next c
	}//next y
        for(y in 1:N) {
            //pred[y] = spawn_pred[y]*exp(alpha[S[y]] - spawn_pred[y]*beta[S[y]]+sum(cov_eff[y,1:ncovars]))*exp(phi[S[y]]); // generating predicted recruits
            //  pred[y] = spawn_pred[y]*exp(alpha[S[y]] - spawn_pred[y]*beta[S[y]]); // generating predicted recruits
            //pred[y] = spawn_pred[y]*exp(alpha[S[y]] - spawn_pred[y]*beta[S[y]])*exp(phi[S[y]]); // generating predicted recruits
          pred[y] = spawn_pred[y]*exp(alpha[S[y]] - spawn_pred[y]*beta[S[y]]+sum(cov_eff[y,1:ncovars]))+1; // generating predicted recruits


        }// next y
    //ln_pred = log(pred); 
}

model {
  // PRIORS
  // alpha hyper priors 
     mu_alpha ~ normal(1,5); // magic number is 3,5
     sigma_alpha ~ normal(0,5); // magic number is 0,5
   // mu_sigma_oe ~ normal(1,0.2); // magic number is 3,5
   //  sigma_sigma_oe ~ normal(0,0.2); // magic number is 0,5
  //   mu ~ normal(7000, 1000);
 //  phi ~ normal(0,0.1);
 // covariate hyper priors 
  for(c in 1:ncovars){
  	 mu_coef[c] ~ normal(0, 10); // 0 - 1 or 0.1 would be a penalized version as a test case - drive it to 0
    sigma_coef[c] ~ normal(0, 5); //0 - 1 or 0.1 would be a penalized version
  	 }
 // alpha, beta, and predicted spawner priors
  for(s in 1:NS) {
    alpha[s] ~ normal(mu_alpha,sigma_alpha);
    beta[s] ~ normal(0.00001,0.01); // magic number 0.00001, and 0.01
    sigma_oe[s] ~ normal(0,0.2);
    for(y in 1:N) {
    spawn_pred[y] ~ lognormal(log(spawn[y]), error_spawn[y]); // vague lognormal prior
   // sigma_oe[S[y]] ~ normal(0,1);
    //sigma_oe[y] ~ normal(0,1);

    //spawn_pred[y] ~ normal(10000,10000); // vague normal prior
   //  spawn_pred[y] ~lognormal(9,0.75);
   }
   //Covariate Effect priors
    for(c in 1:ncovars){
		 theta[s,c] ~ normal(mu_coef[c],sigma_coef[c]);
   }
 }
  
  
// LIKELIHOODS
    for(y in 1:N) {
      //ln_rec[y] ~ normal(log(pred[y]), error_rec[y]+sigma_oe[y]);
        //ln_rec[y] ~ normal(log(pred[y]), sigma_oe[S[y]]);
        ln_rec[y] ~ normal(log(pred[y]), error_rec[y]+sigma_oe[S[y]]);

    }//next y
  }
  
generated quantities {
  real mu_coef_rep[ncovars];
  real ln_rec_new[N];
  //real spawn_new[N];
  // use current estimate of mu_coef to generate new sample
  for (c in 1:ncovars) {
    mu_coef_rep[c] = normal_rng(mu_coef[c],sigma_coef[c]);
   }
    for (y in 1:N) {
    //ln_rec_new[y] = normal_rng(log(pred[y]), error_rec[y]+sigma_oe[y]); //posterior predictive check for recruits
     //ln_rec_new[y] = normal_rng(log(pred[y]), sigma_oe[y]); //posterior predictive check for recruits
     ln_rec_new[y] = normal_rng(log(pred[y]), error_rec[y]+sigma_oe[S[y]]); //posterior predictive check for recruits

  }
}