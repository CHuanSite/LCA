#' Rotate components
#'
#' This function allows you to rotate the components for every simulated component. It is an inner function that does not need to be used by user manually.
#' @param comp The simulated component
#' @param angle The angle to be rotated of the comp
#' @keywords rotation
#' @export

rotate_component <- function(comp, angle = 0){
  rot_mat = matrix(c(cos(angle), sin(angle), -1 * sin(angle), cos(angle)), nrow = 2, ncol = 2)
  return(comp %*% rot_mat)
}


