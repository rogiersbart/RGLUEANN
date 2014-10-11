#' ANN training function
#' 
#' \code{train_ann} is a wrapper for the AMORE package functions
#' 
#' @param Ptrain
#' @param Pval
#' @param targetTrain
#' @param targetVal
#' @param architecture
#' @param n
#' @param Stao
#' @param es
#' @param hidden.layer
#' @param output.layer
#' @return results of training an ANN
train_ann <- function(Ptrain, Pval, targetTrain, targetVal, architecture, n, Stao=1
, es=T, hidden.layer="sigmoid", output.layer="purelin")
{
  neuralnetwork <- newff(n.neurons=architecture, learning.rate.global=1e-2,
  momentum.global=0.5, error.criterium = 'LMS', Stao = Stao, hidden.layer=
  hidden.layer, output.layer=output.layer, method="ADAPTgdwm")
  if(!es) { Pval <- NULL; targetVal <- NULL }     
  capture.output(results <- train(net=neuralnetwork, P=Ptrain, Stao=Stao,  Pval=Pval,
  Tval=targetVal,T=targetTrain, error.criterium = 'LMS', report=T, show.step=1,
  n.shows=n ))
  return(results)
}