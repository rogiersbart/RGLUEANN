#' Make predictions with a GLUE-ANN model ensemble
#' 
#' \code{predict.glue_ann} makes predictions for newdata with a GLUE-ANN model ensemble
#' 
#' @param glue_ann a glue_ann object
#' @param newdata input data frame for making predictions
#' @param weighting GLUE weighting procedure; options are 'MSE', 'MSEes' (for weights based on the early stopping subset performance), 'NSEff' and 'Stedinger'; default: 'Stedinger'
#' @return predictions from glue_ann model ensemble
#' @method predict glue_ann
#' @export
#' @import AMORE
predict.glue_ann <- function(glue_ann,newdata,weighting=glue_ann$parameters$weighting)
{
  #require(AMORE)
  names(newdata) <- c(1:ncol(newdata))
  if(glue_ann$parameters$orthogonal != 'No') newdata <-
  predict(glue_ann$pca, newdata=newdata) # orthogonalise
  for(i in 1:ncol(newdata)) newdata[,i] <-
  (newdata[,i]-mean(glue_ann$P[,i]))/sd(glue_ann$P[,i]) # standardise

  ### Predict model ensemble results for new data ##############################
  resultsMatrix <- matrix(nrow=nrow(newdata), ncol=glue_ann$parameters$nSets)
  if(nrow(newdata) > 1) {for(i in 1:glue_ann$parameters$nSets) {
  resultsMatrix[,i] <- sim(glue_ann$networks[[i]]$net,
  newdata[,glue_ann$variables[[i]]])}}
  if(nrow(newdata) == 1) {for(i in 1:glue_ann$parameters$nSets) {
  resultsMatrix[,i] <- sim(glue_ann$networks[[i]]$net, t(
  newdata[,glue_ann$variables[[i]]]))}}

  ### Construct GLUE weights ###################################################
  if(glue_ann$parameters$weighting=='MSEes'){weightsGLUE <- (max(glue_ann$esMSE)
  - glue_ann$esMSE)
  weightsGLUE[which(weightsGLUE < 0)] <- 0
  weightsGLUE <- weightsGLUE/sum(weightsGLUE)}
  if(glue_ann$parameters$weighting=='MSE'){weightsGLUE <- (
  max(glue_ann$totalMSE) - glue_ann$totalMSE)^2
  weightsGLUE[which(weightsGLUE < 0)] <- 0
  weightsGLUE <- weightsGLUE/sum(weightsGLUE)}
  if(glue_ann$parameters$weighting=='Stedinger')
  {                                                        # ;print(glue_ann$totalVar)
    se2 <- min(glue_ann$totalVar)                          # ;print(se2)
    n <- length(glue_ann$outputData)                       # ; print(n)
    weightsGLUE <- (glue_ann$totalMSE)/(se2)  # ;print(weightsGLUE)  # (-n/2)*
    weightsGLUE <- exp(weightsGLUE) # ;print(weightsGLUE)
  }
  if(glue_ann$parameters$weighting=='NSEff')
  {
    weightsGLUE <- (1-(glue_ann$totalMSE/var(glue_ann$outputData)))
    weightsGLUE[which(weightsGLUE < 0)] <- 0
  }

  ### Weight model predictions #################################################
  predictions <- NULL
  predictions$MLE <- resultsMatrix[1,which.min(glue_ann$totalVar)]    
  if(glue_ann$parameters$weighting != 'Stedinger')
  {
    predictions$eMean <- apply(resultsMatrix, 1, "wtd.mean", weights =
    weightsGLUE, normwt=T)
    predictions$eMedian <- apply(resultsMatrix, 1, "wtd.quantile", weights =
    weightsGLUE, probs = c(0.5), normwt=T)
    predictions$eQ025 <- apply(resultsMatrix, 1, "wtd.quantile", weights =
    weightsGLUE, probs = c(0.025), normwt=T)
    predictions$eQ975 <- apply(resultsMatrix, 1, "wtd.quantile", weights =
    weightsGLUE, probs = c(0.975), normwt=T)
    predictions$eMin <- apply(resultsMatrix, 1, "min")
    predictions$eMax <- apply(resultsMatrix, 1, "max")
    predictions$eResults <- resultsMatrix
  }  
  if(glue_ann$parameters$weighting=='Stedinger')
  {
    resultsMatrix_sted <- matrix(nrow=nrow(newdata), ncol=
    glue_ann$parameters$nSets*glue_ann$parameters$nStedinger)
    for(i in 1:nrow(newdata))
    {
      for(k in 1:glue_ann$parameters$nSets)
      {
        errorVector <- rnorm(glue_ann$parameters$nStedinger,mean=0,sd=sqrt(se2))
        resultsMatrix_sted[i,(((k-1)*glue_ann$parameters$nStedinger)+c(
        1:glue_ann$parameters$nStedinger))] <- errorVector + resultsMatrix[i,k]
      }
    }
    weightsGLUE <- rep(weightsGLUE, each=glue_ann$parameters$nStedinger)
    #print(weightsGLUE)
    predictions$eMean <- apply(resultsMatrix_sted, 1, "wtd.mean", weights =
    weightsGLUE, normwt=T)
    for(i in 1:nrow(newdata))
    {
      quantiles <- wtd.quantile(resultsMatrix_sted[i,], weights = weightsGLUE,
      probs = c(0.025, 0.5, 0.975), normwt=T)
      predictions$eMedian[i] <- quantiles[2]
      predictions$eQ025[i] <- quantiles[1]
      predictions$eQ975[i] <- quantiles[3] 
    }
    predictions$eResults <- resultsMatrix_sted
    predictions$eMin <- apply(resultsMatrix_sted, 1, "min")
    predictions$eMax <- apply(resultsMatrix_sted, 1, "max")    
  }
  
  ### Backtransform of predictions according to training data output ###########
  if(glue_ann$parameters$rescale.output)
  {
    outputRange <- max(glue_ann$outputData)-min(glue_ann$outputData)
    outputMin <- min(glue_ann$outputData)
    predictions$MLE <- predictions$MLE * outputRange + outputMin
    predictions$eMean <- predictions$eMean * outputRange + outputMin
    predictions$eMedian <- predictions$eMedian * outputRange + outputMin
    predictions$eQ025 <- predictions$eQ025 * outputRange + outputMin
    predictions$eQ975 <- predictions$eQ975 * outputRange + outputMin
    predictions$eMin <- predictions$eMin * outputRange + outputMin
    predictions$eMax <- predictions$eMax * outputRange + outputMin
    predictions$eResults <- predictions$eResults * outputRange + outputMin                                
  }
  return(predictions)
}