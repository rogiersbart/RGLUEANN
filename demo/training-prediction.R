# Example of training and prediction for fictional data

  # Create fictional data for x range [1,100]
    inputData <- data.frame(x=seq(1,100,0.5))
    
  # Dependent variable y = x^2                              
    outputData <- inputData$x^2
    
  # add random noise to y, representing different sources of error
    outputData <- jitter(outputData, amount=mean(outputData)*0.2)  
    
  # Run GLUE-ANN functions   
    glue.ann.ensemble <- glue.ann(inputData, outputData, nCycles=500)
    training.results <- predict(glue.ann.ensemble,inputData)
    
  # Visualise results for training data
    plot(inputData$x, training.results$eMean, type='l', xlab='x', ylab=expression(y = x^2 + epsilon), ylim=c(-2000,13000))
    lines(inputData$x, glue.ann.ensemble$outputData, lty=2, col='red')
    lines(inputData$x, training.results$eQ025, lty=3, col='blue')
    lines(inputData$x, training.results$eQ975, lty=3, col='blue')
    legend('topleft', c('GLUE-ANN ensemble mean prediction','Observations','95% prediction uncertainty'), lty=c(1,2,3),col=c('black','red','blue'),bty='n')
    
  # Predict new data
    newData <- data.frame(x=c(-100:200)+0.5)                                       
    newData.predictions <- predict(glue.ann.ensemble, newData)
    
  # Visualise results of prediction
    plot(newData$x, newData.predictions$eMean, type='l', xlab='x', ylab=expression(y = x^2 + epsilon), ylim=c(-2000,13000))                                  
    lines(newData$x, jitter(newData$x^2, amount=mean(outputData)*0.2),lty=2,col='red')
    lines(newData$x, newData.predictions$eQ025, lty=3, col='blue')
    lines(newData$x, newData.predictions$eQ975, lty=3, col='blue')
    abline(v=c(0,100))
    text(50,13000,'Training data range')
    legend('bottomright', c('GLUE-ANN ensemble mean prediction','Observations','95% prediction uncertainty'), lty=c(1,2,3),col=c('black','red','blue'),bty='n')
