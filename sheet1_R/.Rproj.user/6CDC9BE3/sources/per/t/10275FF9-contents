###########
########### Sheet 1
###########

################################ Problem 1
# b)
n <- 10000
x <- 0:4
p <- c(0.1, 0.2, 0.2, 0.2, 0.3)
Fx <- cumsum(p)

set.seed(42)
u <- runif(n)

sample_inverse <- function(u){
  min(x[u<=Fx])
}

sample1 <- sapply(u, sample_inverse)


# c)
rel_freq_table1 <- table(sample1)/n

hist(sample1)
round(rbind(rel_freq_table1, p), 3)

# d)
sample2 <- sample(x, size = n, replace = TRUE, prob = p)

freq_table2 <- table(sample2)/n

hist(sample2, probability  = TRUE)

# ggplot2
library(ggplot2)
ggplot(data = data.frame(sample2),mapping = aes(x = sample2, y = stat(density))) +
  geom_histogram(binwidth = 1, color = "white")



################################ Problem 2
# b) 
inverse_cdf <- function(u, sigma){
  return(sigma*sqrt(-2*log(u)))
}

generate_rv <- function(n,sigma){
  u <- runif(n)
  return(inverse_cdf(u,sigma))
}

# c)
n = 1000
# sd = 0.5
sigma = c(0.5, 1, 2, 4)

samples <- lapply(sigma, function(sigma) generate_rv(n, sigma))

rayleigh_data <- data.frame(sigma = rep(sigma, each = n), 
                            rayleigh = unlist(samples))

factor(rep(sigma, each = n))
ggplot(rayleigh_data, aes(x = rayleigh, color = factor(sigma))) + 
  # Add density
  geom_density() +
  # Add vertical lines corresponding to sigma
  geom_vline(aes(xintercept = sigma, color =  factor(sigma)), 
             linetype = "dotted") + 
  # Change labels
  labs(title = "Density estimates for\ndifferent values of sigma", x = "x",
       color =  expression(sigma))


################################ Problem 2
# b) & c)
n <- 1000
num_acc <- 0
iteration <- 0
samples <- numeric(n)

while(num_acc < n){
 u <- runif(1)
 g <- runif(1)
 if(u <= 12 * g*(1-g)^4){
   num_acc <- num_acc + 1
   samples[num_acc] <- g
 }
 iteration <- iteration + 1
}

iteration

ggplot(data = data.frame(samples), mapping = aes(samples)) +
  geom_histogram(aes(y = stat(density)), color = "red") +
  ggtitle("Sample histogram with the \nBeta(2,5)-density superimposed") + 
  stat_function(fun = dbeta,
                args = list(shape1 = 2, shape2 = 5), color = "blue", size = 1)



################################ Problem 4

sample_25 <- rexp(25, rate = 0.5)
sample_52 <- rexp(52, rate = 0.5)

ggplot(data = data.frame(sample_25), mapping = aes(sample_25)) +
  geom_histogram(aes(y = stat(density)),color = "white") +
  stat_function(fun = dexp,
                args = list(rate = 0.5), color = "blue", size = 1)

ggplot(data = data.frame(sample_52), mapping = aes(sample_52)) +
  geom_histogram(aes(y = stat(density)),color = "white") +
  stat_function(fun = dexp,
                args = list(rate = 0.5), color = "blue", size = 1)


################################ Problem 5
fun_sample <- function(pi, B, alpha){
  return(qnorm(1-alpha/2)^2 * (pi*(1-pi))/B^2)
}

p <- ggplot(data.frame(pi = c(0,1)), aes(pi)) + 
  stat_function(fun = fun_sample, args = list(B = 0.03, alpha = 0.05)) 
p
