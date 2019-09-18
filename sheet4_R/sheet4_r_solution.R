###########
########### Sheet 4
###########

################################ Problem 1
# given data: it represents the times in hours between failures of a machine

times <- c(3, 5, 7, 18, 43, 85, 91, 98, 100, 130, 230, 487)

# a)
lambda_mle <- function(obs){
  return(1/mean(obs))
}



# b)
# Here we use the parametric bootstrap:
# estimate the bias
# estimate the mean squared error
perform_parametric_bootstrap <- function(m = 10, B, lambda = 0.001){
  
  sample_matrix <- matrix(rexp(m*B, rate= lambda), nrow = B, ncol= m)
  
  bootstrap_estimates <- apply(sample_matrix, MARGIN = 1, function(x) 1/mean(x))
  
  bootstrap_lambda <- mean(bootstrap_estimates)
  
  bootstrap_bias <- bootstrap_lambda - lambda
  
  bootstrap_MSE <- mean((bootstrap_estimates - lambda)^2)
  
  return(list(bootstrap_lambda = bootstrap_lambda,
              bootstrap_bias = bootstrap_bias,
              bootstrap_MSE = bootstrap_MSE))
}

perform_parametric_bootstrap(B = 200)

# c)
# Here we use the non-parametric bootstrap
B <- 200
m <- length(times)

non_p_bootstrap_estimates <- replicate(B, 1/mean(sample(x=times, size = m, replace = TRUE)))

non_p_bootstrap_bias <- mean(non_p_bootstrap_estimates) - lambda_mle(times)
ste <- sd(non_p_bootstrap_estimates - lambda_mle(times))

################################ Problem 2
height <- c(173, 183, 187, 179, 180, 186, 179, 196, 202, 198, 197,
            185, 194, 185, 191, 182, 182, 187, 184, 186)
B <- 200

n <- length(height)

# b)
# part b) is to determine the confidence intervals and part
library(ggplot2)

mean_height <- mean(height)
mean_bootstrap <- replicate(B, mean(sample(x=  height, size = n, 
                                           replace = TRUE)))

## standard normal bootstrap
std_normal <- c(mean_height - qnorm(1-0.05/2)*sd(mean_bootstrap),
                mean_height + qnorm(1-0.05/2)*sd(mean_bootstrap))

## basic bootstrap
#mean_bs_sorted <- sort(mean_bootstrap)
l <- quantile(mean_bootstrap, probs = 1-0.05/2)
u <- quantile(mean_bootstrap, probs = 0.05/2)

basic_bts <- c(2*mean_height - l, 2*mean_height - u)

## bootstrap percentile
l <- quantile(mean_bootstrap, probs = 0.05/2)
u <- quantile(mean_bootstrap, probs = 1 - 0.05/2)

bts_percentile <- c(l,u)

## bootstrap t

t <- numeric(length = B)

mean_bootstrap <- numeric(length = B)

for(b in 1:B){
  x_b <- sample(height, size = length(height), replace = TRUE)
  
  mean_bootstrap[b] <- mean(x_b)
  
  bts_means <- replicate(B, mean(sample(x=  x_b, size = length(height), 
                           replace = TRUE)))
  t[b] <- (mean(x_b) - mean_height)/sd(bts_means)
  
}

l <- quantile(t, probs = 1 - 0.05/2)
u <- quantile(t, probs = 0.05/2)

bts_t <-c(mean_height - l*sd(mean_bootstrap), mean_height - u*sd(mean_bootstrap))

# c)
intervals <- seq(min(height), max(height), by = 5)
sapply(intervals, function(x) sum(height < x)/length(height))

test <- ecdf(height)
test(intervals)

# d)

# use ggplot we have to define data frames
conf_ints <- data.frame(
  type = c("standard_normal", "basic", "percentile", "t"),
  lower = c(std_normal[1], basic_bts[1], bts_percentile[1], bts_t[1]),
  upper = c(std_normal[2], basic_bts[2], bts_percentile[2], bts_t[2])
)

ggplot_histogram <- ggplot(data = data.frame(means = mean_bootstrap), aes(x = means)) +
  geom_histogram(aes(y = stat(density)), color = "white", alpha = 0.5)

ggplot_histogram + geom_vline(data = conf_ints,
                             aes(xintercept = lower, color = type, linetype = type), size = 1) +
  geom_vline(data = conf_ints,
             aes(xintercept = upper, color = type, linetype = type), size = 1) +
  geom_vline(aes(xintercept = mean(means)), size = 1,color = "blue")

std_normal
basic_bts
bts_percentile
bts_t


mean.height <- mean_height
ci.data.list <- replicate(B, {
  x.samples <- sample(height, size = n, replace = TRUE)
  theta.hat <- mean(x.samples)
  theta.hat.rep <- replicate(B, mean(sample(x.samples, size = n, replace = TRUE)))
  # Return a data.frame with theta.hat and the t value
  data.frame(theta.hat, t.value = (theta.hat - mean.height) / sd(theta.hat.rep))
}, simplify = FALSE)
dim(ci.data.list)

help(replicate)
