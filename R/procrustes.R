#' Procrustes Projection
#'
#' This function allows you to compute the procrustes projections, min_U ||A - UB||^2 U^T U = I. It is an inner function, which shouldn't be used by users alone.
#' @param A The objective matrix to be approximated
#' @param B The matrix to be used to approximate
#' @keywords procrustes

Procrustes <- function(A, B){
  C = A %*% t(B)
  svd.temp = svd(C)
  return(svd.temp$u %*% t(svd.temp$v))
}
