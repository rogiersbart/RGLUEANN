#' Train a GLUE-ANN model ensemble
#' 
#' \code{glue_ann} trains a GLUE-ANN model ensemble. A dataframe has to be provided with the independent variable(s), and a vector with the dependent variable. A list of parameters has to be set to define the a priori distributions of the stochastic parameters, and fixed values for the ones considered to be constant.
#' 
#' @param inputData input data frame
#' @param outputData output data frame
#' @param orthogonal character; orthogonalize the input data; options are: 'Cov', 'Cor', and 'No'; default: 'Cor'
#' @param variableNumber number of input variables; provide vector for stochastic treatment; defaults to number of columns of inputData
#' @param dimRed logical; TRUE selects always most variance explaining PCs; FALSE chooses PCs randomly; default: TRUE
#' @param rescale.output logical; rescale output to [0,1] range; default: TRUE
#' @param ES logical; enable early stopping; default: TRUE
#' @param randomESPart part of the data used for early stopping; default: 0.5
#' @param nonEqualMeans force similar mean for the early stopping samples as for the training samples; default: TRUE
#' @param maxMeanDiff maximum difference between early stopping and training observation means; defaults to 20 percent of the target data mean
#' @param nCycles maximum number of training cycles, provide a vector for stochastic treatment; default: 100
#' @param HLrange amount of hidden layers, provide vector for stochastic treatment; default: c(1:1)
#' @param HNrange amount of hidden nodes, provide vector for stochastic treatment; default: c(1:10)
#' @param maxHNPerVar maximum amount of hidden nodes per input variable; default: Inf
#' @param hidden.layer hidden layer activation function; options are 'tansig', 'sigmoid', 'purelin'; default: 'tansig'
#' @param output.layer output layer activation function; options are 'tansig', 'sigmoid', 'purelin'; default: 'sigmoid'
#' @param nSets number of models in the ensemble; default: 100
#' @param nStedinger number of random variates for weights based on Stedinger et al. (2008); default: 50
#' @param weighting GLUE weighting procedure; options are 'MSE', 'MSEes' (for weights based on the early stopping subset performance), 'NSEff' and 'Stedinger'; default: 'Stedinger'
#' @param cv logical; do leave-one-out cross-validation; default: FALSE
#' @return Object of class glue_ann
#' @export
#' @import AMORE Hmisc
glue_ann <- function(inputData,outputData,orthogonal='Cor',variableNumber=
c(1:ncol(inputData)),dimRed=T,rescale.output=T,ES=T,randomESPart=0.5,
nonEqualMeans=T,maxMeanDiff=mean(outputData)*0.2,nCycles=100,HLrange=c(1:1),
HNrange=c(1:10),maxHNPerVar=Inf,hidden.layer='tansig',output.layer='sigmoid',
nSets=100,nStedinger=50,weighting='Stedinger',cv=F)
{
  ### Initialisation ###########################################################
  #require(AMORE)
  #require(Hmisc)
  parameters <- NULL
  parameters$orthogonal <- orthogonal
  parameters$variableNumber <- variableNumber
  parameters$dimRed <- dimRed
  parameters$rescale.output <- rescale.output
  parameters$ES <- ES
  parameters$randomESPart <- randomESPart
  parameters$nonEqualMeans <- nonEqualMeans
  parameters$maxMeanDiff <- maxMeanDiff
  parameters$nCycles <- nCycles
  parameters$HLrange <- HLrange
  parameters$HNrange <- HNrange
  parameters$maxHNPerVar <- maxHNPerVar
  parameters$hidden.layer <- hidden.layer
  parameters$output.layer <- output.layer
  parameters$nSets <- nSets
  parameters$nStedinger <- nStedinger
  parameters$weighting <- weighting
  parameters$cv <- cv        
  glue_ann <- NULL
  cvPredictions <- NULL
  glue_ann$parameters <- parameters      
  glue_ann$esNumber <- matrix(nrow=nrow(inputData), ncol=nSets)
  
  ### Training loop ############################################################
  if(cv) cat('Starting cross-validation loop..\n')
  for(crossValidationSampleNumber in 1:ifelse(cv,nrow(inputData),1))
  {
    if(cv)
    { 
        inputDataModellingCV <- as.data.frame(inputData[crossValidationSampleNumber,])
        inputDataModelling <- as.data.frame(inputData[-crossValidationSampleNumber,])
        outputDataModellingCV <- outputData[crossValidationSampleNumber]
        outputDataModelling <- outputData[-crossValidationSampleNumber] 
    } else {
        inputDataModelling <- inputData
        outputDataModelling <- outputData
    }
    names(inputDataModelling) <- c(1:ncol(inputDataModelling))
    if(cv) names(inputDataModellingCV) <- c(1:ncol(inputDataModellingCV))        
    glue_ann$inputData <- inputDataModelling
    glue_ann$outputData <- outputDataModelling
    P <- cbind(inputDataModelling)    
    if(orthogonal != 'No'){Ppca <- princomp(P, cor=ifelse(orthogonal=='Cor', T, F));P <- Ppca$scores}
    trans <- P
    glue_ann$P <- P
    for(i in 1:ncol(inputData)) trans[,i] <- (trans[,i]-mean(P[,i]))/sd(P[,i])           
    if(rescale.output) outputDataModelling <- (outputDataModelling - (min(glue_ann$outputData)))/(((max(glue_ann$outputData))-(min(glue_ann$outputData))))
    if(rescale.output & cv) outputDataModellingCV <- (outputDataModellingCV -(min(glue_ann$outputData)))/(((max(glue_ann$outputData))-(
    min(glue_ann$outputData))))
    variableslist <- list();networks <- list(); esNumber <- NULL
    earlyStoppingMSE <- NULL; earlyStoppingCycle <- NULL; hn <- list()
    totalVar <- NULL; trainingMSE <- NULL
    cat('Starting training loop..\n')
    for(j in 1:nSets)
    {
      if(cv) cat(paste('# Cross-validating sample',crossValidationSampleNumber,'\n'))
      if(ES) cat('Selecting early stopping data..\n')
      esDataNumbers <- sample(1:nrow(inputDataModelling),round(nrow(inputDataModelling)*randomESPart,0))
      while(nonEqualMeans)
      {
        esDataNumbers <- sample(1:nrow(inputDataModelling),round(nrow(inputDataModelling)*randomESPart,0))
        esMean <- mean(outputDataModelling[esDataNumbers])
        trainMean <- mean(outputDataModelling[-esDataNumbers])
        if(abs(esMean-trainMean) < maxMeanDiff) nonEqualMeans <- F
      }        
      trainingDataNumbers <- c(1:nrow(inputDataModelling))[-esDataNumbers]
      if(!ES)
      {
        trainingDataNumbers <- c(1:nrow(inputDataModelling))
        esDataNumbers <- trainingDataNumbers
      }
      cat('Selecting variables and ANN architecture..\n')
      nvars <- resample(variableNumber,1)
      variables <- resample(c(1:ncol(inputDataModelling)),nvars)
      if(dimRed) variables <- c(1:length(variables))
      variableslist[[j]] <- variables
      hiddenLayers <- resample(HLrange,1, replace=T)
      hiddenNodes <- resample(HNrange,hiddenLayers, replace=T)
      for(i in 1:hiddenLayers) if(maxHNPerVar < hiddenNodes[i]/nvars) {
      hiddenNodes[i] <- resample((min(HNrange):maxHNPerVar),1, replace=T)}
      ncyc <- resample(nCycles,1)
      cat('# Training ANN nr. ',j,', with architecture: ',sep='')
      cat(c(nvars,hiddenNodes[1:hiddenLayers],1), sep='-')
      cat('\n')
      Ptrain <- as.data.frame(trans[trainingDataNumbers,variables]   )
      Pval <- as.data.frame(trans[esDataNumbers,variables])
      targetTrain <- outputDataModelling[trainingDataNumbers]
      targetVal <- outputDataModelling[esDataNumbers]
      networks[[j]] <- train_ann(Ptrain, Pval, targetTrain, targetVal,
      architecture=c(nvars,hiddenNodes,1), ncyc, Stao=10, es=ES, hidden.layer=
      hidden.layer, output.layer=output.layer)
      if(ES) esNumber[j] <- which.min(networks[[j]]$Merror[,2])
      predTrain <- sim(networks[[j]]$net, Ptrain)
      trainingMSE[j] <- mean((targetTrain-predTrain)^2)
      if(ES) predEarlyStopping <- sim(networks[[j]]$net, Pval)
      if(ES) earlyStoppingMSE[j] <- mean((targetVal-predEarlyStopping)^2)
      if(ES) totalVar[j] <- var(c(predTrain-targetTrain, predEarlyStopping-
      targetVal))
      if(!ES) totalVar[j] <- var(c(predTrain-targetTrain))
      if(ES) earlyStoppingCycle[j] <- which.min(networks[[j]]$Merror[,2])
      hn[[j]] <- hiddenNodes
      if(ES) cat(paste('# Training MSE: ',format(trainingMSE[j],scientific=T, digits=3),
      ', early stopping MSE: ',format(earlyStoppingMSE[j],scientific=T, digits=3),', cycles: ',
      earlyStoppingCycle[j],'\n', sep=''))
      if(!ES) cat(paste('# Training MSE: ',format(trainingMSE[j],scientific=T, digits=3),
      ', cycles: ',earlyStoppingCycle[j],'\n', sep=''))
    }
    glue_ann$networks <- networks
    glue_ann$variables <- variableslist
    glue_ann$trainMSE <- trainingMSE
    if(ES) glue_ann$esMSE <- earlyStoppingMSE
    if(ES) glue_ann$totalMSE <- (1-randomESPart) * glue_ann$trainMSE +
    randomESPart * glue_ann$esMSE
    if(!ES) glue_ann$totalMSE <- glue_ann$trainMSE
    glue_ann$totalVar <- totalVar
    glue_ann$hn <- hn
    if(orthogonal != 'No') glue_ann$pca <- Ppca
    class(glue_ann) <- 'glue_ann'
    
    ### Predict cross-validated sample #########################################
    if(cv)
    {
      cvPrediction <- predict(glue_ann, inputDataModellingCV)
      cvPredictions$MSE[crossValidationSampleNumber] <- mean(
      (outputData[crossValidationSampleNumber] - cvPrediction$eMean)^2)
      cvPredictions$eMean[crossValidationSampleNumber] <- cvPrediction$eMean
      cvPredictions$eMedian[crossValidationSampleNumber] <- cvPrediction$eMedian
      cvPredictions$eMin[crossValidationSampleNumber] <- cvPrediction$eMin
      cvPredictions$eMax[crossValidationSampleNumber] <- cvPrediction$eMax
      cvPredictions$eQ025[crossValidationSampleNumber] <- cvPrediction$eQ025
      cvPredictions$eQ975[crossValidationSampleNumber] <- cvPrediction$eQ975
      cvPredictions$MLE[crossValidationSampleNumber] <- cvPrediction$MLE
      cvPredictions$target[crossValidationSampleNumber] <- outputData[crossValidationSampleNumber]
    }
  }
  if(cv){return(cvPredictions)} else {return(glue_ann)}
}