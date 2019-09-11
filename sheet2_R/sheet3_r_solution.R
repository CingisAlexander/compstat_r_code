###########
########### Sheet 1
###########

################################ Problem 2
library(ggplot2)
# a)
# generall setting
n <- 1000

# Generate 1000 samples of specific normal distribution
X1 <- rnorm(n, mean = 0, sd = 1)
X2 <- rnorm(n, mean = 4, sd = 1)

S <- X1 + X2
# Plot the histogram with the expected density N(4,1) 
p <- ggplot(data = data.frame(S = S), aes(x = S)) + 
  geom_histogram(aes(y = stat(density)), color = "darkcyan", fill = "darkcyan",
                 alpha = 0.5) +
  stat_function(fun = dnorm, args = list(mean = 4, sd = sqrt(2)), size = 1)+
  scale_x_continuous(breaks = seq(-2,10,2))

p

hist(S, probability = TRUE)

# b)
# Here usage of facet_wrap is presented
n <- 1000
p = c(0.1, 0.4, 0.6, 0.8)

generate_gaussian_mixture <- function(p){
  step1 <- sample(c(1, 0), size = n, replace= TRUE, prob = c(p,1-p))
  random_mixture <- step1 *rnorm(1000, mean = 0, sd = 1) + (1-step1)*rnorm(1000, mean = 4, sd = 1)
}

random_mixtures <- lapply(p, generate_gaussian_mixture)

# create nice readable plot

mix_samples <- data.frame(p = rep(p, each = n),x = unlist(random_mixtures))
ggplot(mix_samples, aes(x)) + geom_density() +
  #split the plot by different probabilities
  facet_wrap(~p,labeller = label_both)


################################ Problem 3
# general setting
lambda <- c(0.5, 1, 1.5, 2, 2.5, 3)
theta <- c(0.1, 0.1, 0.2, 0.2, 0.3, 0.1)
n <- 5000

# a)
# the expextation of Gamma mixture
expectation <- (sum(theta*(3/lambda)))

variance <- sum(3*(theta/lambda^2)+9*(theta/lambda^2)) - expectation^2

# b)
step1 <- sample(x = 1:6, size = n, replace = TRUE, prob = theta)

mix_samples <- sapply(step1, FUN = function(idx){return(rgamma(1,shape = 3, rate = lambda[idx]))})

# alternative solution
rate <- sample(x = lambda, size = n, replace = TRUE, prob = theta)
mix_samples <- rgamma(n = n, shape = 3, rate = rate)

p <- ggplot(data = data.frame(x = mix_samples), aes(x)) + geom_density(color = "red", size = 1)

for(lambda_i in lambda){
  p <- p + stat_function(fun = dgamma, args = list(shape = 3, rate = lambda_i),
                         linetype = "dashed")
}
p

################################ Problem 3
# d)

generate_multinorm <- function(n, p, mu, Sigma){
  M <- matrix(mu, n, p, byrow = TRUE)
  
  Z <- matrix(rnorm(n*p), nrow = n, ncol = p)
  
  Q <- chol(Sigma)
  
  X <- Z%*%Q + M
  return(X)
}

n <- 1000
mu <- c(1,3,0)
sigma <- matrix(c(1, -.8, -.5, -.8, 1, .2, -.5, .2, 1), nrow = 3,
                ncol = 3,byrow =  TRUE)

X200 <- generate_multinorm(n = n, p = 3, mu = mu, Sigma = sigma)

library(GGally)
ggpairs(data.frame(X200))

