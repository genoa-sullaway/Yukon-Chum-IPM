
K=1
k=1
t=2
A=4
a=1
set.seed(123)

kappa_j  =  matrix(nrow = nByrs, ncol = K,NA)  
kappa_marine  = matrix(nrow = nByrs, ncol = K,NA)  
N_j  =  matrix(nrow = nByrs, ncol = K,NA)
N_recruit = matrix(nrow = nByrs, ncol = K,NA)
N_returning = array(data = NA, dim = c(nByrs, K,A))
N_sp = array(data = NA, dim = c(nByrs, K,A))
N_e = array(data = NA, dim = c(nByrs, K,A))
cov_eff1= array(data = NA, dim = c(nByrs, K,1))
cov_eff2= array(data = NA, dim = c(nByrs, K,1))
p_1=matrix(nrow = nByrs, ncol = K,NA)
p_2=matrix(nrow = nByrs, ncol = K,NA)

N_e_sum = matrix(nrow = nByrs, ncol = K, NA)
N_e_sum[1,1]<-exp(rnorm(1,30,10))

# p_1  =  matrix(nrow = nByrs, ncol = K, 0.2)  
# p_2  = matrix(nrow = nByrs, ncol = K, 0.4) 
basal_p_1 = 0.2
basal_p_2 = 0.4

# covariates test ==========
#set.seed(123)
mu_coef1 <- rnorm(1,1,1)
sigma_coef1 <- rnorm(1,5,5)
theta1 <- rnorm(1,mu_coef1,sigma_coef1)
theta1

mu_coef2 <- rnorm(1,1,1)
sigma_coef2 <- rnorm(1,5,5)
theta2 <- rnorm(1,mu_coef2,sigma_coef2)
theta2

for (t in 1:nByrs) {
      cov_eff1[t,1,1] = theta1 *cov1[t,1]
      cov_eff2[t,1,1] = theta2*cov2[t,1]

      p_1[t,k]  = 1 / exp(-basal_p_1-sum(cov_eff1[t,1,1]))
      p_2[t,k]  = 1 / exp(-basal_p_2 -sum(cov_eff2[t,1,1]))
   }

 
p = matrix(nrow = nRyrs, ncol = A, NA)

p[,1:A] <- c(0.4, 0.3, 0.2,0.1)
p[2,1:A] <- c(0.4, 0.3, 0.2,0.1)
p[5,1:A] <- c(0.4, 0.3, 0.2,0.1)
p[6,1:A] <- c(0.4, 0.3, 0.2,0.1)

set.seed(123)
c_1 = rnorm(1,1e8, 1e1)
c_2 = rnorm(1,1e6, 1e4)
catch_q = rnorm(1,2,10)
 
H_b <- cbind(data_stage_harvest,o_run_comp_mat) %>%
  as.data.frame() %>% 
  dplyr::mutate(a3 = V1*age3,
                a4 = V1*age4,
                a5 = V1*age5,
                a6 = V1*age6) %>%
  dplyr::select(6:9) %>%
  as.matrix()

H_b_array <- array(data = H_b, dim = c(nRyrs, K,A))

# for(k in 1:K){  #// loop for each population
#   for (t in 2:nByrs){ #//this should maube be 1??
#     
t=4
      kappa_j[t,k] =  p_1[t,k]/ (1 + ((p_1[t,k]*N_e_sum[t-1,k])/c_1[k])) #// Eq 4.1  - Bev holt transition estimating survival from Egg to Juvenile (plugs into Eq 4.4) 
      kappa_j[t,k]
      
      N_j[t,k] = kappa_j[t,k]*N_e_sum[t-1,k] #// Eq 4.4  generated estiamte for the amount of fish each year and stock that survive to a juvenile stage
      N_j[t,k]
      
      kappa_marine[t,k] =  p_2[t,k]/ (1 + ((p_2[t,k]*N_j[t,k])/c_2[k])) #// Eq 4.1   - Bev holt transition estimating survival from juvenile to spawner (plugs into Eq 4.4) 
      kappa_marine[t,k]      
      
      N_recruit[t,k] = kappa_marine[t,k]*N_j[t,k] #// Eq 4.5 generated estiamte for the amount of fish each year and stock that survive to a spawning stage
      N_recruit[t,k]
    #  //add in age for stage where I am tracking age class.... 
    #  //for (c in 1:nRyrs) { // swtich to calendar years using t+A-a
      
        for (a in 1:A) { 
          N_returning[t+A-a,k,a] = (catch_q[k]*N_recruit[t,k])*p[t+A-a,a]  #// currys indexing here: N_ta[t,a] = R[t+A-a] * p[t+A-a,a];
          N_returning[t+A-a,k,a]
          
          N_sp[t+A-a,k,a] = N_returning[t+A-a,k,a] - H_b_array[t+A-a,k,a]  
          
          N_e[t+A-a,k,a] = fs[a,1]*Ps*N_sp[t+A-a,k,a] #// Eq 4.3 generated estimate for the amount of eggs produced that year for that stock.
        
       }
       
      # // transition back to brood years - plug in ages manually
        N_e_sum[t,k] = (N_e[t+A-1,k,1]+N_e[t+A-2,k,2]+N_e[t+A-3,k,3]+N_e[t+A-4,k,4])
        N_e_sum[t,k] #= N_e[t+A-1,k,1]
   
        
        
        
        
        