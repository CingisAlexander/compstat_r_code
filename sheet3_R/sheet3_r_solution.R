###########
########### Sheet 3
###########

################################ Problem 1

# f)

mc_unif <- function(m){
  # this function estimates the give integral using unif distribution.
  # output is a list where first elem is a the integral estimation, 
  # second is the variance of the estimation
  
  # sample from unifom distr
  sample1 <- runif(m, min = 0, max = 0.5)
  
  # estimate  the integral:
  integral_est <- 0.5*mean(exp(-sample1))
  
  # estimate the var
  var_est <- (1/m)*var(0.5*exp(-sample1))
  return(list("integral" = integral_est, "var_est" = var_est))
}

mc_exp<- function(m){
  # this function estimates the give integral using exponential distribution.
  # output is a list where first elem is a the integral estimation, 
  # second is the variance of the estimation
  
  # We sample from exponential distribution
  sample1 <- rexp(m, rate = .5)
  
  # Now estimate the integral
  integral_est <- mean(sample1 <= 0.5)
  
  # Now, estimate the variance of our estimate
  var_est <- (1/m)*var(sample1 <= 0.5)
  return(list("integral" = integral_est, "var_est" = var_est))
}


sample_mc_unif <- mc_unif(100)

sample_mc_exp <- mc_exp(100)

exact_int <- 1 - exp(-0.5)

# g)
library(ggplot2)

# Numberof iterations
n <- 100

# Number of samples in each iteration
m <- 100

# Create a matrix with n*m random samples of U(0, 0.5)
x1_sample <- matrix(runif(m*n, min = 0, max= 0.5), nrow = n, ncol = m)
# Apply the MC-Unif to each row
theta_hat_vec <- apply(x1_sample, MARGIN = 1, function(x){return(0.5*mean(exp(-x)))})
var_theta_hat_vec <- apply(x1_sample, MARGIN = 1, function(x){return(var(0.5*exp(-x))/m)})

# Create a matrix with n*m random samples of Exp(1)
x2_samples <- matrix(rexp(n = n*m, rate = 1),n,m)
# Apply the MC-Expto each row
theta_star_vec <- apply(x2_samples, MARGIN = 1, function(x){return(mean(x <= .5))})
var_theta_star_vec <- apply(x2_samples, MARGIN = 1, function(x){return(var(x <= .5)/m)})

## Now, we create the violin plots
MC_data <- data.frame(MC_type = rep(c("theta_hat_vec", "theta_star_vec"), each = n),
                      theta = c(theta_hat_vec, theta_star_vec),
                      var_theta <- c(var_theta_hat_vec, var_theta_star_vec))

# Use the MC_type as fill-color and group_variable
ggplot(data = MC_data, mapping = aes(MC_type, var_theta, fill = MC_type, group = MC_type)) +
  # add violin plot
  geom_violin() + 
  geom_boxplot(fill = "white", width = 0.2) +
  # Split the plot by MC_type with free scales to better visualize the distribution
  facet_wrap(~MC_type, scales = "free")
# The facet_wrap splits one plot intro several plots by one categorical variable.
# So in the violin plot we can see the the "x axis" represent type of MC method, i.e.
# theta_hat_vec and theta_star_vec. And on the y-axis we can see the the computed variace-theta
# with its boxplot and kernel density. As we can see the kernel density gives us in the case
# of theta_star_vec more infromation, we see the distribution is parially bi-modal.

################################ Problem 1

# c) importance sampling:
# g is the function that we want to integrate over on intgral [1, inf)
g <- function(x){
  return((x^2*exp(-x^2/2))/sqrt(2*pi)*(x>1))
}

# f is the importance function
f <- function(x){
  return(exp(-x)/exp(-1)*(x>1))
}

F_inv <- function(u){
  return(1 -log(u))
}

estimate_integral_and_std_dev <- function(m){
  # we use the inverse transform method to determine the samples
  # of f
  set.seed(25)
  sample1 <- runif(m)
  sample2 <- F_inv(sample1)
  integral <- mean(g(sample2)/f(sample2))
  std_dev <- sqrt(var(g(sample2)/f(sample2)))
  return(list(integral = integral, std_dev = std_dev))
}

estimate_integral_and_std_dev(10000)
