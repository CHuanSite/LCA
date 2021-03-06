#' generate simulated data
#'
#' This function allows you to generate simulated data
#' @param configuration_setting The output of the configuration_setting_generation function
#' @keywords simulation
#' @export
#' @examples
#' configuration_setting = configuration_setting_generation()
#' data_list = simulated_data_generation(configuration_setting)

simulated_data_generation <- function(configuration_setting){
  ## Setting up configurations
  featureNum = configuration_setting$featureNum
  DataNum = configuration_setting$DataNum
  commonlySharedNum = configuration_setting$commonlySharedNum
  partiallySharedNum = configuration_setting$partiallySharedNum
  individualSharedNum = configuration_setting$individualSharedNum
  noiseVariance = configuration_setting$noiseVariance
  commonComponent = configuration_setting$commonComponent
  partialComponent = configuration_setting$partialComponent
  individualComponent = configuration_setting$individualComponent

  ## Rotate each component
  A11 = rotate_component(commonComponent, 0)
  A12 = rotate_component(commonComponent, 0)
  A21 = rotate_component(commonComponent, 0)
  A22 = rotate_component(commonComponent, 0)

  B11 = rotate_component(partialComponent[[1]], 0)
  B12 = rotate_component(partialComponent[[1]], 0)
  B21 = rotate_component(partialComponent[[2]], 0)
  B22 = rotate_component(partialComponent[[2]], 0)

  C11 = rotate_component(partialComponent[[3]], 0)
  C12 = rotate_component(partialComponent[[4]], 0)
  C21 = rotate_component(partialComponent[[3]], 0)
  C22 = rotate_component(partialComponent[[4]], 0)

  D11 = rotate_component(individualComponent[[1]], 0)
  D12 = rotate_component(individualComponent[[2]], 0)
  D21 = rotate_component(individualComponent[[3]], 0)
  D22 = rotate_component(individualComponent[[4]], 0)

  F1 = score_generation(commonlySharedNum, DataNum[1], c(60, 80))
  F2 = score_generation(commonlySharedNum, DataNum[2], c(50, 70))
  F3 = score_generation(commonlySharedNum, DataNum[3], c(60, 80))
  F4 = score_generation(commonlySharedNum, DataNum[4], c(50, 60))

  G1 = score_generation(partiallySharedNum[1], DataNum[1], c(30, 40))
  G2 = score_generation(partiallySharedNum[1], DataNum[2], c(45, 60))
  G3 = score_generation(partiallySharedNum[2], DataNum[3], c(50, 70))
  G4 = score_generation(partiallySharedNum[2], DataNum[4], c(30, 65))

  H1 = score_generation(partiallySharedNum[3], DataNum[1], c(30, 40))
  H2 = score_generation(partiallySharedNum[4], DataNum[2], c(50, 60))
  H3 = score_generation(partiallySharedNum[3], DataNum[3], c(45, 35))
  H4 = score_generation(partiallySharedNum[4], DataNum[4], c(75, 55))

  K1 = score_generation(individualSharedNum[1], DataNum[1], c(40, 50))
  K2 = score_generation(individualSharedNum[2], DataNum[2], c(60, 70))
  K3 = score_generation(individualSharedNum[3], DataNum[3], c(30, 40))
  K4 = score_generation(individualSharedNum[4], DataNum[4], c(50, 60))

  E1 <- matrix(rnorm(featureNum * DataNum[1], 0, noiseVariance[1]), nrow = featureNum)
  E2 <- matrix(rnorm(featureNum * DataNum[2], 0, noiseVariance[2]), nrow = featureNum)
  E3 <- matrix(rnorm(featureNum * DataNum[3], 0, noiseVariance[3]), nrow = featureNum)
  E4 <- matrix(rnorm(featureNum * DataNum[4], 0, noiseVariance[4]), nrow = featureNum)
  #
  Y1 = A11 %*% F1 + B11 %*% G1 + C11 %*% H1 + D11 %*% K1 + E1
  Y2 = A12 %*% F2 + B12 %*% G2 + C12 %*% H2 + D12 %*% K2 + E2
  Y3 = A21 %*% F3 + B21 %*% G3 + C21 %*% H3 + D21 %*% K3 + E3
  Y4 = A22 %*% F4 + B22 %*% G4 + C22 %*% H4 + D22 %*% K4 + E4

  return(list(Y1, Y2, Y3, Y4))
}
