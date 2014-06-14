# Example of cross-validation with an R dataset

  # Read input and output data (ozone concentration in function of wind, temperature and solar radiation)
    outputData <- na.omit(airquality)[,1]
    inputData <- na.omit(airquality)[,2:4]
    
  # Perform cross-validation
    cross_validation <- glue.ann(inputData, outputData, cv=T, nCycles=1000, nSets=10)
    
  # Visualisation
    plot(cross_validation$target, ylim=c(-100,200), xlab='Sample number', ylab='Ozone')
    points(cross_validation$eMean, pch=2, col='red')
    lines(cross_validation$eQ025, lty=3, col='blue')
    lines(cross_validation$eQ975, lty=3, col='blue')
    legend('bottomright', c('Observations','GLUE-ANN ensemble mean prediction','95% prediction uncertainty'), pch=c(1,2,NA), lty=c(NA,NA,3),col=c('black','red','blue'),bty='n')
