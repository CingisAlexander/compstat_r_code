# compstat_r_code
This repo contains r code solutions to computational statistics

In this readme I introduce some nice features from are that were used in computational statistics or to visualize the results


#### Sheet 5; Permuatation test

- Implementation of exact permutations test using the function *combinations* from the package *gtools*. Can be found in **Problem 1** 

- The approximate permutation test was also implemented. To plot *ggplots* next to each other use *ggarrange* from *ggpubr*. 
To mulitple functions such as ecdfs you can use *stat_function*. Can be found in **Problem 2**

- Scatterplot where each dot is labeled and how to specify the ticks of x or y axis, **Problem 3**

- **Nice template to fit multiple different linear models with a k fold cross validation**. With *dplyr* transform data into
longformat and with ggplot plot multiple boxplots regarding errors of different linear models in one plot.


#### Sheet 6; Metropolis Hastings

- Implementation of the metrpolis hastings sampler to sample from Rayleigh distribution, where as proposal density
$Gamma(X_t,1)$ is used. Lastly the samples are plotted as time series plot with ggplot. **Problem 1**

- Implementation of metrpolis independence sampler to sample from 2-component mixture model, as proposal density is the
uniform distribution used. **Problem 3**

- Fancy plots where multiple MCMC chains are plotted next to each other, each chain is differently colored.
The meaning of the color is then decoded a plot agenda. **Problem 3**

- Gelman-Rubin Statistics is computed. **Problem 3**