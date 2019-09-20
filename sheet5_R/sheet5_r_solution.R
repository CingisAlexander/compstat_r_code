###########
########### Sheet 5
###########

# Keywords:
# Implementation of permuation test using the gtools package for creating all combinations
# implementation using for-loop, and an efficient way with apply, in exam you
# dont have to know apply!
# ggplot of histograms and ggarrange from ggpubr to plot the histograms next to each other 
# how to plot multiple ecdfs, nice intro: http://t-redactyl.io/blog/2016/03/creating-plots-in-r-using-ggplot2-part-9-function-plots.html

# Problem 3 c) ggplot how to plot scatter plot where each dot is labeled, and how to specify the ticks of x or y axis

# Problem 4 c) introduces a nice template to fit multiple different linear models
# with a k fold cross validation. Lasty, with ggplot visualize  the cross validation
# with boxplots and remove a legend from a plot
################################
################################ Problem 1 ####


# a)
library(gtools)
library(ggplot2)

# Implement the exact permutation test
control <- c(12, 17, 14, 9)
drug <- c(23, 18, 26, 21)

c_d <- c(control, drug)

# all possible combinations
combi <- combinations(8,4)

# num of combinations
num_com <- 70 

# this vector will contain statistic evaluated for each combination
stat_value_vec_mean <- numeric(num_com)
stat_test_mean <- numeric(num_com)

# this is for the median case
stat_value_vec_median <- numeric(num_com)
stat_test_median <- numeric(num_com)

stat_value_mean <- mean(drug) - mean(control)
# this is for median case
stat_value_median <- median(drug) - median(control)

for(ct in 1:num_com){
  comb <- combi[ct,]
  g1 <- c_d[comb]
  g2 <- c_d[-comb]
  # mean case
  stat_value_vec_mean[ct] <- mean(g2)-mean(g1)
  stat_test_mean[ct] <- abs(mean(g2)-mean(g1)) >=abs(stat_value_mean)
  
  # median
  stat_value_vec_median[ct] <- median(g2)-median(g1)
  stat_test_median[ct] <- abs(median(g2)-median(g1)) >=abs(stat_value_median)
}

(asl_mean <- mean(stat_test_mean))
(asl_median <- mean(stat_test_median))

# So in the case of mean we reject the null hypothesis since the asl_mean < 0.05
# But in the case of median we can not reject since asl > 0.05

# use ggpubr to plot plots next to each other
library(ggpubr)

histogram_mean <- ggplot(data = data.frame(stat_value_mean = stat_value_vec_mean), aes(x = stat_value_mean)) +
  geom_histogram(color = "white", binwidth = 0.5) + 
  geom_vline(xintercept = stat_value_mean, color = "blue") +
  ggtitle("Possible mean differences")

histogram_median <- ggplot(data = data.frame(stat_value_median = stat_value_vec_median), aes(x = stat_value_median)) +
  geom_histogram(color = "white", binwidth = 0.5) +
  geom_vline(xintercept = stat_value_median, color = "green") +
  ggtitle("Possible median differences")

ggarrange(histogram_mean, histogram_median)

# In both cases we can reject the null hypothesis via the permuation test since, 
# since the p-values were really small.

test <- combinations(70, 12)

############################ Efficient way using apply function
############################
stat_value_vec_mean <- apply(combi, MARGIN = 1, function(x) mean(c_d[-x]) - mean(c_d[x]))
asl <- mean(abs(stat_value_vec_mean) >= abs(stat_value_mean))



################################
################################ Problem 2 ####

# a) 
factorial(71)/(factorial(12)^3 * factorial(10) * factorial(11) * factorial(14))

# b)
# What is one-way analysis of variance (ANOVA) is an extension of independent
# two-sample t-test for comparing means. Thus, we use it in cases where we have more than
# two groups.
# Null hypothesis: the means of different groups are the same
# Alternative hypothesis: at least one mean is different

summary(aov(weight ~ feed, chickwts))
F_value <-summary(aov(weight ~ feed, chickwts))[[1]][1, "F value"]

# From the summary we can reject the null hypothesis

# c)

num_replicates <- 1000

num_samples <- nrow(chickwts)
num_groups <- length(unique(chickwts$feed))

mean_casein <- mean(chickwts$weight[chickwts$feed == "casein"])
casein <- sum(chickwts$feed == "casein")

mean_horsebean <- mean(chickwts$weight[chickwts$feed == "horsebean"])
horsebean <- sum(chickwts$feed == "horsebean")

mean_linseed <- mean(chickwts$weight[chickwts$feed == "linseed"])
linseed <- sum(chickwts$feed == "linseed")

mean_meatmeal <- mean(chickwts$weight[chickwts$feed == "meatmeal"])
meatmeal <- sum(chickwts$feed == "meatmeal")

mean_soybean <- mean(chickwts$weight[chickwts$feed == "soybean"])
soybean <- sum(chickwts$feed == "soybean")

mean_sunflower <- mean(chickwts$weight[chickwts$feed == "sunflower"])
sunflower <- sum(chickwts$feed == "sunflower")

init_stat_value <- mean_casein + mean_horsebean + mean_linseed - mean_meatmeal - mean_soybean - mean_sunflower

matrix_mean_stats <- matrix(numeric(num_replicates*(num_groups+1)), nrow= num_replicates, ncol=(num_groups+1))


for(row_num in 1:num_replicates){
  elem_casein <- 1:num_samples
  casein_combi <- sample(x=elem_casein, size=casein, replace = FALSE)
  mean_casein_perm <- mean(chickwts$weight[casein_combi])
  
  elem_horsebean <- elem_casein[-casein_combi]
  horsebean_combi <- sample(elem_horsebean, horsebean, replace = FALSE)
  mean_horsebean_perm <- mean(chickwts$weight[horsebean_combi])
  
  elem_linseed <- elem_horsebean[-horsebean_combi]
  linseed_combi <- sample(elem_linseed, linseed, replace = FALSE)
  mean_linseed_perm <- mean(chickwts$weight[linseed_combi])
  
  elem_meatmeal <- elem_linseed[-linseed_combi]
  meatmeal_combi <- sample(elem_meatmeal, meatmeal, replace = FALSE)
  mean_meatmeal_perm <- mean(chickwts$weight[meatmeal_combi])
  
  elem_soybean <- elem_meatmeal[-meatmeal_combi]
  soybean_combi <- sample(elem_soybean, soybean, replace = FALSE)
  mean_soybean_perm <- mean(chickwts$weight[soybean_combi])
  
  sunflower_combi <- elem_soybean[-soybean_combi]
  mean_sunflower_perm <- mean(chickwts$weight[sunflower_combi])
  
  mean_perm <- c(mean_casein_perm, mean_horsebean_perm, mean_linseed_perm,
                 mean_meatmeal_perm, mean_soybean_perm, mean_sunflower_perm)
  stats <- sum(c(-mean_casein_perm, -mean_horsebean_perm, -mean_linseed_perm,
                 mean_meatmeal_perm, mean_soybean_perm, mean_sunflower_perm))
  
  matrix_mean_stats[row_num,] <- c(mean_perm, stats)
}

# p-value:
(p_value <- sum(abs(matrix_mean_stats[,7]) >=  abs(init_stat_value))/num_replicates)

############################ Right solution
############################
# in approximate permutation test means
# to use instead the asl, the test statistic, e.g. in our case the 
# the F-statistic
set.seed(102)
num_replicates <- 1000

replicate_F_stat <- replicate(num_replicates, {
  feed_sample <- sample(chickwts$feed)
  f_stat_summary <- summary(aov(chickwts$weight ~ feed_sample))
  return(f_stat_summary[[1]][1, "F value"])
})

(p_value_approx <- mean(abs(replicate_F_stat) > abs(F_value) ))

ggplot(data = data.frame(x = replicate_F_stat), aes(x, stat(density))) +
  geom_histogram(color = "white", binwidth = 0.2) +
  geom_density() +
  geom_vline(xintercept = F_value)

# e)
# First we plot the ecdfs
linseed <- c(141, 148, 169, 181, 203, 213, 229, 244, 257, 260, 270, 309)
soybean <- c(158, 171, 193, 199, 230, 243, 248, 248, 250, 267, 271, 316, 327, 329)

ecdf_linseed <- ecdf(linseed)
ecdf_soybean <- ecdf(soybean)
  

test <- function(x){return(x^2)}  
  
min_x <- min(c(linseed, soybean))
max_x <- max(c(linseed, soybean))

p <- ggplot(data.frame(x = c(min_x, max_x)), aes(x = x)) +
  stat_function(fun = ecdf_linseed, aes(color = "linseed")) +
  stat_function(fun = ecdf_soybean, aes(color = "soybean"))
p

# Now perform the kolmogorov smirnov test statistic
ks.test(linseed, soybean)

# f)
num_replicates <- 1000

num_groups <- 2

all_samples <- c(linseed, soybean)

num_samples <- length(all_samples)

mean_linseed <- mean(linseed)
linseed_len <- length(linseed)

mean_soybean <- mean(soybean)
soybean_len <- length(soybean)

init_stat_value <- mean_linseed - mean_soybean

matrix_mean_stats <- matrix(numeric(num_replicates*(num_groups+1)), nrow= num_replicates, ncol=(num_groups+1))

for(row_num in 1:num_replicates){
  elem_linseed <- 1:num_samples
  linseed_combi <- sample(elem_linseed, linseed_len, replace = FALSE)
  mean_linseed_perm <- mean(all_samples[linseed_combi])

  mean_soybean_perm <- mean(all_samples[-linseed_combi])
  
  
  mean_perm <- c(mean_linseed_perm,
                 mean_soybean_perm)
  
  stats <- mean_linseed_perm - mean_soybean_perm
  
  matrix_mean_stats[row_num,] <- c(mean_perm, stats)
}

# p-value:
p_value <- sum(abs(matrix_mean_stats[,3]) >=  abs(init_stat_value))/num_replicates

p_histogram <- ggplot(data = data.frame(stat_value = matrix_mean_stats[,3]), aes(x = stat_value)) +
  geom_histogram()

################################
################################ Problem 3) ####
library(ggplot2)
library(ggpubr)
LSAT <- c(576, 635, 558, 578, 666, 580, 555, 661, 651, 605, 653, 575, 545, 572, 594)
GPA <- c(339, 330, 281, 303, 344, 307, 300, 343, 336, 313, 312, 274, 276, 288, 296)

# a)
n <- length(LSAT)

theta_hat <- cor(LSAT, GPA)

theta_hat_jack <- sapply(1:n, function(i) cor(LSAT[-i], GPA[-i]))

theta_bar <- mean(theta_hat_jack)

(bias_jack <- (n-1)*(theta_bar - theta_hat))

(se_jack <- sqrt(((n-1)/n)*sum((theta_hat_jack - theta_bar)^2)))


# Now, we plot the theta_hat_jack
(p_hist <- ggplot(data = data.frame(theta_hat_jack), aes(x=theta_hat_jack)) +
  geom_histogram(color="white", binwidth = 0.01) +
  geom_vline(xintercept = theta_bar, color = "blue") +
  geom_vline(xintercept = theta_hat, color = "red"))

# In the plot we can see there is some point that causes 
# high correlations. Thus, we would like to find this point

(p_scatter <- ggplot(data = data.frame(lsat = LSAT, gpa = GPA, observation = as.character(1:n)),
                    aes(x = gpa, y = lsat, label = observation))+
    geom_text())
  
(p_scatter1 <- ggplot(data = data.frame(observation = 1:n, theta_hat_jack),
                     aes(x = theta_hat_jack, y = observation)) +
  geom_point() +
    scale_y_continuous(breaks = seq(1, n, by = 2)))

ggarrange(p_scatter, p_scatter1)



################################
################################ Problem 4) ####
library(DAAG)

#### b) ####
# There should be in total 26 groups, where for instance the last groups will have three
# test data

# We create firstly set of groups
groups <- c(rep(1:26, each = 2), 26)
ironslag$groups <- sample(groups)
group <- 1

error <- sapply(1:26, function(group){
  test_data <- ironslag[ironslag$groups %in% group, ]
  training_data <- ironslag[!ironslag$groups %in% group, ]
  

  model <- lm(magnetic ~ chemical, data = training_data)
  
  predict_test <- predict(model, newdata = test_data)
  
  error <- mean((test_data$magnetic - predict_test)^2)
})

mean(error)


#### c) ####
set.seed(487)
groups <- c(rep(1:26, 2), 26)

ironslag$groups <- sample(groups)


# The following returns a list of data.frames
error_list <- lapply(1:26, function(group_id){
  training_data <- ironslag[ !ironslag$groups %in% group_id,]
  test_data <- ironslag[ ironslag$groups %in% group_id,]
  
  models <- list(
    lin_model <- lm(magnetic ~ chemical, data = training_data),
    quad_model <- lm(magnetic ~ chemical + I(chemical^2), data = training_data),
    exp_model <- lm(log(magnetic) ~ chemical, data = training_data),
    loglog_model <- lm(log(magnetic) ~ log(chemical), data = training_data)
  )
  
  # predict y_hat
  y_hats <- lapply(models, function(model) return(predict(model, newdata = test_data)))
  
  # take the exponential of y hats for exp_model and loglog_model
  y_hats[3:4] <- lapply(y_hats[3:4], exp)
  
  error_list <- lapply(y_hats, function(y) mean((test_data$magnetic - y)^2))
  names(error_list) <- c("lin_model", "quad_model", "exp_model", "loglog_model")
  
  as.data.frame(error_list)
})

# Now, from a list back to dataframe with reduce
(errors <- Reduce(rbind, error_list))

# Calculate MSE for the prediction
sapply(errors, mean)

# Calculate the standard deviations 
sapply(errors, sd)


###### Visualize the dta using box plot 
library(tidyverse)

long_errors <- gather(errors, model, test_error) 

ggplot(long_errors, aes(model, test_error, fill = model)) +
  geom_boxplot() +
  theme(legend.position = "none")
