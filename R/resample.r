#' Sampling parameter vectors function
#' 
#' \code{nncalc} is a wrapper for the AMORE package functions
#' 
#' @param x vector to sample from
#' @param n number of samples to draw from x
#' @param ... other arguments to pass to sample.int
#' @return results of training an ANN
resample <- function(x, n, ...)
{
  x[sample.int(length(x), size=n, ...)]
}
