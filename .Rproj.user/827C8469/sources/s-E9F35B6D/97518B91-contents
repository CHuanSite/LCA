#' configuration setting generations
#'
#' This function allows you to set configuration settings manually.
#' @param featureNum The dimension of the features.
#' @param DataNum The number of samples of each dataset.
#' @keywords configuration
#' @export
#' @examples
#' configuration_setting_generation()

configuration_setting_generation <- function(featureNum = 50, DataNum = c(100, 100, 100, 100)){

  ## Number of components used
  commonlySharedNum = 2
  partiallySharedNum = c(2,2,2,2)
  individualSharedNum = c(2,2,2,2)

  ## Noise variance
  noiseVariance = c(1,1,1,1)

  tempRandomMatrix = matrix(rnorm(featureNum * (sum(commonlySharedNum, partiallySharedNum, individualSharedNum))), nrow = featureNum)
  svdRandomMatrix = svd(tempRandomMatrix)

  ## Common Component
  commonComponent = svdRandomMatrix$u[, 1 : commonlySharedNum]

  ## Partially shared component
  partialComponent = list()
  partialComponent[[1]] = svdRandomMatrix$u[, (commonlySharedNum + 1) : (commonlySharedNum + partiallySharedNum[1])]
  partialComponent[[2]] = svdRandomMatrix$u[, (commonlySharedNum + partiallySharedNum[1] + 1) : (commonlySharedNum + partiallySharedNum[1] + partiallySharedNum[2])]
  partialComponent[[3]] = svdRandomMatrix$u[, (commonlySharedNum + partiallySharedNum[1] + partiallySharedNum[2] + 1) : (commonlySharedNum + partiallySharedNum[1] + partiallySharedNum[2] + partiallySharedNum[3])]
  partialComponent[[4]] = svdRandomMatrix$u[, (commonlySharedNum + partiallySharedNum[1] + partiallySharedNum[2] + partiallySharedNum[3] + 1) : (commonlySharedNum + partiallySharedNum[1] + partiallySharedNum[2] + partiallySharedNum[3] + partiallySharedNum[4])]

  ## Individual Component
  individualComponent = list()
  individualComponent[[1]] = svdRandomMatrix$u[, (sum(commonlySharedNum, partiallySharedNum) + 1) : (sum(commonlySharedNum, partiallySharedNum) + individualSharedNum[1])]
  individualComponent[[2]] = svdRandomMatrix$u[, (sum(commonlySharedNum, partiallySharedNum) + individualSharedNum[1] + 1) : (sum(commonlySharedNum, partiallySharedNum) + individualSharedNum[1] + individualSharedNum[2])]
  individualComponent[[3]] = svdRandomMatrix$u[, (sum(commonlySharedNum, partiallySharedNum) + individualSharedNum[1]+ individualSharedNum[2] + 1 ): (sum(commonlySharedNum, partiallySharedNum) + individualSharedNum[1] + individualSharedNum[2] + individualSharedNum[3])]
  individualComponent[[4]] = svdRandomMatrix$u[, (sum(commonlySharedNum, partiallySharedNum) + individualSharedNum[1]+ individualSharedNum[2] + individualSharedNum[3] + 1 ): (sum(commonlySharedNum, partiallySharedNum) + individualSharedNum[1] + individualSharedNum[2] + individualSharedNum[3] + individualSharedNum[4])]


  return(list(featureNum = featureNum, DataNum = DataNum, commonlySharedNum = commonlySharedNum,
              partiallySharedNum = partiallySharedNum, individualSharedNum = individualSharedNum,
              noiseVariance = noiseVariance, commonComponent = commonComponent, partialComponent = partialComponent,
              individualComponent = individualComponent))
}
