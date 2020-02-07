#' Score generation
#'
#' This function allows you to random generate scores to be used. It is a inner function that users do not need to use manually.
#' @param dim_score The dimension of the scores.
#' @param num_score How many scores needed.
#' @param score_variance The variance of each scores.
#' @keywords score

score_generation <- function(dim_score, num_score, score_variance){
  sim_cov = diag(score_variance)
  sim_mean = rep(0, dim_score)
  return(t(MASS::mvrnorm(num_score, sim_mean, sim_cov)))
}

