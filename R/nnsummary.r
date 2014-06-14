################################################################################
# FUNCTION - nnsummary #########################################################
################################################################################
nnsummary <- function(Ptrain, Pval, targetTrain, targetVal, results, printing=T)
{
  y1 <- sim(results$net, Ptrain)
  yVal1 <- sim(results$net, Pval)
  #upscaling1 <- sim(results$net, Pupscaling)
  if(printing)
  {
    print('Training MSE')
    print(round(mean((targetTrain-y1)^2),3))
    print('Validation MSE')
    print(round(mean((targetVal-yVal1)^2),3))
    #print('Dessel 2 MSE')
    #print(round(mean((targetUpscaling-upscaling1)^2),3))
    print('Optimal generalization cycle number')
    print(which(results$Merror[,2]==min(results$Merror[,2])))
  }
  return(c(round(mean((targetTrain-y1)^2),3),round(mean((targetVal-yVal1)^2),3),which(results$Merror[,2]==min(results$Merror[,2]))))
}