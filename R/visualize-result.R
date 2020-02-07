#' Visualization function
#'
#' This function allows you to visualize the result of LCA
#' @param dataset A list containing all the datasets.
#' @param group A list containing the linked groups of datasets.
#' @param factor_num A vector containing the number of factors each group should have.
#' @param linked_component_list A list containing the estimated linked component.
#' @param score_list A list containing the list of scores.
#' @param dataset_exp A list containing the explanation arguments that can be used to visualize the result
#' @keywords visualization
#' @export

visualize_result <- function(dataset, group, factor_num, linked_component_list, score_list, dataset_exp){
  ## Parameters to be initialized
  N = length(dataset)
  K = length(group)
  M = sum(factor_num)
  p = nrow(dataset[[1]])

  for(k in 1 : K){
    if(length(group[[k]]) == 4){
      par(mfrow = c(2,2))
    }else{
      par(mfrow = c(1,2))
    }
    for(i in 1 : N){
      if(i %in% group[[k]]){
        plot(t(score_list[[i]][[k]]), xlab = "PC1", ylab = "PC2", col = dataset_exp[[i]]$color, main = dataset_exp[[i]]$name, pch = 16)
      }
    }
  }

}
