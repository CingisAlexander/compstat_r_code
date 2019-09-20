# compstat_r_code
This repo contains r code solutions to computational statistics

In this readme I introduce some nice features from are that were used in computational statistics or to visualize the results


#### Sheet 4

- Implementation of exact permutations test using the function *combinations* from the package *gtools*. Can be found in **Problem 1** 

- The approximate permutation test was also implemented. To plot *ggplots* next to each other use *ggarrange* from *ggpubr*. 
To mulitple functions such as ecdfs you can use *stat_function*. Can be found in **Problem 2**

- Scatterplot where each dot is labeled and how to specify the ticks of x or y axis, **Problem 3**

- **Nice template to fit multiple different linear models with a k fold cross validation**. With *dplyr* transform data into
longformat and with ggplot plot multiple boxplots regarding errors of different linear models in one plot.
