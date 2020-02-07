# LCA
A R package for linked component analysis, which is a visualized tool for multiple datasets. 

## Functions 

```configuration_setting_generation```: This function generates the configuration settings to be used in the simulations.

```simulated_data_generation```: This function generates the simulation data by taking the output list of ```configuration_setting_generation``` function as its input argument.

```LCA```: Main function of this package, take a list of datasets and structures shared between those datasets. Output the computed linked component and corresponding scores.

```visualize_result```: This function takes output of the LCA and visualize the random scores computed by it with colors,  useful in the genomics studies with some known information.

## Example

Install the packages 
```
library(devtools)
install_github("CHuanSite/LCA")
```

Generate the simulated data
```
configuration_setting = configuration_setting_generation(50, c(100, 100, 100, 100))
dataset = simulated_data_generation(configuration_setting)
```

Prespecify the arguments to be used in the linked component analysis computation:
```
group = list(c(1,2,3,4), c(1,2), c(1,4), c(2,3), c(3,4), c(1), c(2), c(3), c(4))
factor_num = c(2, 2, 2, 2, 2, 2, 2, 2, 2)
```

Compute the linked component from the datasets
```
lca = LCA(dataset, group, factor_num)
```


