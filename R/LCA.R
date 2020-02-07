#' Linked Component Analysis(LCA) function.
#'
#' This function allows you to implement the main algorithm LCA
#' @param dataset A list containing all the datasets
#' @param group A list containing the linked groups of datasets
#' @param factor_num A vector contains the number of factors each group should have
#' @param T The maximum number of iterations allowed to have in the algorithm
#' @keywords LCA
#' @export
#' @examples
#' configuration_setting = configuration_setting_generation(50, c(100, 100, 100, 100))
#' dataset = simulated_data_generation(configuration_setting)
#' group = list(c(1,2,3,4), c(1,2), c(1,4), c(2,3), c(3,4), c(1), c(2), c(3), c(4))
#' factor_num = c(2, 2, 2, 2, 2, 2, 2, 2, 2)
#' LCA(dataset, group, factor_num)

LCA <- function(dataset, group, factor_num, T = 100){
  ## Parameters to be initialized
  N = length(dataset)
  K = length(group)
  M = sum(factor_num)
  p = nrow(dataset[[1]])

  ## Combine the dataset into a huge one
  combine_data <- c()
  for(i in 1 : N){
    combine_data = cbind(combine_data, dataset[[i]])
  }

  ## List to store the random scores and initialize the scores
  score_list = list()
  for(i in 1 : N){
    score_list[[i]] = list()
  }
  for(i in 1 : N){
    for(j in 1 : K){
      if(i %in% group[[j]]){
        score_list[[i]][[j]] = matrix(runif(factor_num[j] * ncol(dataset[[i]])), nrow = factor_num[j])
      }else{
        score_list[[i]][[j]] = matrix(0, nrow = factor_num[j], ncol = ncol(dataset[[i]]))
      }
    }
  }

  ## Start the Alternative Projection
  for(t in 1 : T){
    matrix_score = c()
    for(i in 1 : N){
      temp_score = 0
      for(j in 1 : K){
        temp_score = rbind(temp_score, score_list[[i]][[j]])
      }
      matrix_score = cbind(matrix_score, temp_score)
    }

    ## Apply Procrustes projection to obtain the linkedin component
    linked_component = Procrustes(combine_data, matrix_score)
    linked_component_list = list()
    index = 1
    for(i in 1 : K){
      linked_component_list[[i]] = linked_component[, index : (index + factor_num[i] - 1)]
      index = index + factor_num[i]
    }

    ## Compute the random scores for every dataset
    for(i in 1 : N){
      for(j in 1 : K){
        if(i %in% group[[j]]){
          score_list[[i]][[j]] = t(linked_component_list[[j]]) %*% dataset[[i]]
        }else{
          score_list[[i]][[j]] = matrix(0, nrow = factor_num[j], ncol = ncol(dataset[[i]]))
        }
      }

    }
    print(t)
  }

  return(list(linked_component_list = linked_component_list, score_list = score_list))
}