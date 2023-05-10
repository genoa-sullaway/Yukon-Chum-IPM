par=pars_start # starting values for parameter estimations 
 #NLL is function that you create above 
# data/fixed values go below
N_yi=N_yi
B_yj=B_yj
obs_N=obs_N
obs_escape_project=obs_escape_project
#values
weeks=weeks
projects=projects
Nyear=Nyear
weights = weights





# Load the Baranov catch equation function
baranov <- function(q, Z, B, C) {
  C - q * B * (1 - exp(-Z))
}

# Define the negative log-likelihood function for the Baranov catch equation
nll <- function(par, data) {
  q <- par[1]
  Z <- par[2]
  B <- data$B
  C <- data$C
  residuals <- baranov(q, Z, B, C)
  -sum(dnorm(residuals, mean = 0, sd = data$sigma, log = TRUE))
}

# Generate some data from the Baranov catch equation with known parameters
set.seed(123)
q_true <- 0.5
Z_true <- 0.2
B_true <- 1000
sigma <- 10
C <- q_true * B_true * (1 - exp(-Z_true)) + rnorm(1, mean = 0, sd = sigma)

# Define the data as a list
data <- list(B = B_true, C = C, sigma = sigma)

# Use optim to find the maximum likelihood estimates for q and Z
fit <- optim(par = c(0.1, 0.1), fn = nll, data = data, method = "BFGS")
fit


# _________________________-



# Define the log-likelihood function
loglik <- function(params, y_up, y_down, y_catch) {
  # Unpack the parameters
  lambda <- params[1]
  sigma <- params[2]
  
  # Calculate the estimated total return
 n_hat <- lambda * (y_up + y_down + y_catch)
  
  # Calculate the log-likelihood
  ll <- sum(dlnorm(y_up + y_down + y_catch, log(lambda), sigma, log = TRUE))
  
  return(-ll)  # negative log-likelihood for optimization
}

# Define the data
y_up <- 1000   # observed number of chum salmon migrating upstream of Kalskag
y_down <- 500   # estimated escapement for the tributaries downstream of Kalskag
y_catch <- 200   # estimated subsistence and commercial catches

# Define the starting values for the parameters
params0 <- c(100, 1)

# Run the optimization
fit <- optim(params0, loglik, y_up = y_up, y_down = y_down, y_catch = y_catch)

# Print the optimized parameters and log-likelihood
cat("Optimized parameters:")
cat("\nlambda =", fit$par[1])
cat("\nsigma =", fit$par[2])
cat("\nLog-likelihood =", -fit$value)
