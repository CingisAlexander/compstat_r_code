###########
########### Sheet 6
###########

# Content:
# Problem1/b) draw from the Rayleigh(4) distribution using the Metropolis-Hastings sampler
# where the proposal density is Gamma(X, 1).
# Whit stat_function we plot multiple distributions in one plot.
# subset we canr reduce the number of rows so that the final time series plot
# is easier to grasp

# Problem3/c) nice ggplot with histograms and its estimated density
# together with another denisities, that hav different colors. 
# The name of the densities are in a legend decoded with its color.

# Problem3 we generate using mcmc independence sampler random samples from a posterior distribution
# We run multiple chaing with different staring values to have cover the support of the density
# of the posterior distribution. Als a case where we do not have enough samples for the 
# posterior distribution and what are its causes
# with ggplot plot multiple time series. 
# Gelman rubin statistics is used to check if the chains has converged.

################################
################################ Problem 1 ####
library(ggplot2)


#### a) ####
drayleigh <- function(x){
  (x/4^2)*exp((-x^2)/(2*4^2))
}
set.seed(159)
## Initialize the variables:
# Number of iterations
N <- 10000

# Here we be the obtained random samples stored
xt <- numeric(N)

# First values is going to be the mode of rayleigh(4)
xt[1] <- 4

# Number of acceptance
k <- 0

# We draw at once from the uniform distribution
u <- runif(N)
for(t in 2:N){
  # Previous step:
  xt_prev <- xt[t-1]
  
  # Proposal value from the proposal density:
  y <- rgamma(n =1, shape = xt_prev, rate = 1)
  
  numerator <- drayleigh(y)*dgamma(xt_prev, shape = y, rate = 1)
  denominator <- drayleigh(xt_prev)*dgamma(y, shape = xt_prev, rate = 1)
  
  if(u[t]<= (numerator/denominator)){
    xt[t] <- y
    k <- k +1
  } else{
    xt[t] <- xt_prev
  }
}

# what is the acceptance rate:
(acc_rate <- k/N)

metropolis <- data.frame(value = xt, step = 1:N)
metropolis <- metropolis[-(1:0.2*N), ]
# we show the realization of the chain in a timeseris plot
(p_time_series <- ggplot(data = data.frame(value = xt, step = 1:N), aes(x=step, y=value))+
  geom_line())


# take only each tenth iterate
metropolis_reduced <- subset(metropolis, step %in% seq(1,N,10))

(p_time_series <- ggplot(data = metropolis_reduced, aes(x=step, y=value))+
    geom_line() + labs(title = "Displaying every 10th iterate"))


p1 <- ggplot(data = data.frame(x = c(min(xt), max(xt))), aes(x)) +
  stat_function(fun = dgamma, args = list(shape = 4.5, rate = 1), color = "blue") +
  stat_function(fun = drayleigh, color = "red") +
  stat_function(fun = dchisq, args = list(df = 2), color = "black")
  
p1
# as we can se the rayleigh and denisties are close to each other 
# for this reason the acceptance rate is higher than in the case of 
# chi square.



################################
################################ Problem 3 ####

#### c) ####
set.seed(197)
n <- 1000 # if you are in e) set here n = 20
p <- 0.3
chi <- c(2, 10)
first_sample <- sample(x = 1:2, size = n, prob = c(p, 1-p), replace = TRUE)
mixture_sample <- rchisq(n, df =chi[first_sample])


(plot_hist_dens <- ggplot(data = data.frame(x = mixture_sample),aes(x = x)) +
  geom_histogram(aes(y = stat(density))) +
    stat_function(aes(color= "chi-square df = 2"),
                  fun = dchisq, args = list(df = 2))+
    stat_function(aes(color= "chi-square df = 10"),
      fun = dchisq, args = list(df = 10)) +
    stat_function(aes(color = "mixture density"),
                  fun = function(x) p*dchisq(x,2)+(1-p)*dchisq(x,10))+
    ylim(c(0,0.15)) +
    theme(legend.title = element_blank()))


#### d) ####
# We write a function that generates the chain which will follow the 2-component mixture
dposterior <- function(p = 0.1, sample){
  p*dchisq(sample, df = 2) + (1-p)*dchisq(sample, df = 10)
}

generate_mcmc_chain <- function(p_start,N, sample){
  
  xt <- numeric(N)
  xt[1] <- p_start
  k <- 0
  u <- runif(N)
  for(t in 2:N){
    # prosal value
    y <- runif(1)
    
    xt_prev <- xt[t-1]
    nominator <- dposterior(y, sample)
    denominator <- dposterior(xt_prev, sample)
    r <- prod(nominator/denominator)
    if(u[t] <= r){
      xt[t] <- y
    }else{
      xt[t] <- xt_prev
    }
  }
  
  return( xt)
}


N <- 10000
p_init <- c(0.1, 0.5, 0.9)

set.seed(68)
chains <- lapply(p_init, generate_mcmc_chain, N = N, sample= mixture_sample)

indep_sampler_data <- data.frame(index = rep(1:N, 3),
                                samples = unlist(chains),
                                startup = rep(p_init, each = N))

# plot all chains together to see if one differ from the others
ggplot(indep_sampler_data, aes(index, samples, color = factor(startup))) +
  geom_line() +
  labs(color = "starting values")
# all three chains intersect each other.


indep_sampler_wo_burn_in <- indep_sampler_data[indep_sampler_data$index > 100,]

ggplot(indep_sampler_wo_burn_in, aes( index, samples, color = factor(startup)))+
  geom_line() +
  facet_wrap(~startup, nrow = 3) + 
  labs(color = "starting values")

aggregate(indep_sampler_wo_burn_in$samples,
          by = list(indep_sampler_wo_burn_in$startup),
          mean)

# we can state the the chain has converged all three chain has value close
# to 0.3 


#### e) ####


#### f) ####

scalar_summary <- sapply(chains, function(x) cumsum(x)/seq_along(x))
scalar_summary <- t(scalar_summary)

gelman_rubin_stats <- function(psi){
  psi <- as.matrix(psi)
  n <- nrow(psi)
  
  psi_row_mean <- rowMeans(psi)
  B <- n*var(psi_row_mean)
  
  psi_row_var <- apply(psi, MARGIN = 1, var)
  W <- mean(psi_row_var)
  var_hat <- (n/n-1)*W + (B/n)
  r_hat <- var_hat/W
  return(r_hat)
}

r_hat <- gelman_rubin_stats(scalar_summary)

# now we plot the devepolment of the statistic

r_hat_development <- sapply(1:N, function(t) 
  gelman_rubin_stats(scalar_summary[,1:t]))

ggplot(data = data.frame(r_hat = r_hat_development, t = 1:N),
       aes(x = t,y = r_hat))+
  geom_line() +
  geom_hline(yintercept = 1.2, linetype = "dashed")
