################################################################################
# FUNCTION - nnplot ############################################################
################################################################################
nnplot <- function(results, n, plottitle='')
{
  par(mar=c(5,4,4,5)+.1)
  plot(1:n,results$Merror[,1],type="l",col="red", xlab="Number of training cycles", ylab="Training set MSE", main=plottitle, log='y')
  par(new=TRUE)
  plot(1:n,(results$Merror[,2]),type="l",col="blue",xaxt="n",yaxt="n",xlab="",ylab="", log='y')
  abline(v=c(1:n)[which.min(results$Merror[,2])], lty=2, col='black', cex=1.5)
  axis(4)
  mtext("Validation set MSE",side=4,line=3)   # , cex=1.5
  legend("topright",col=c("red","blue","black"),lty=c(1,1,2),legend=c("Training set","Validation set", "Early stopping point"),bty='n')
}