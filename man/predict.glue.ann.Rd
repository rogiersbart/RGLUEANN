\name{predict.glue.ann}
\alias{predict.glue.ann}
\title{Make predictions with a GLUE-ANN model ensemble}
\usage{
\method{predict}{glue.ann}(glue.ann, newdata,
  weighting = glue.ann$parameters$weighting)
}
\arguments{
  \item{glue.ann}{a glue.ann object}

  \item{newdata}{input data frame for making predictions}

  \item{weighting}{GLUE weighting procedure; options are
  'MSE', 'MSEes' (for weights based on the early stopping
  subset performance), 'NSEff' and 'Stedinger'; default:
  'Stedinger'}
}
\value{
predictions from glue.ann model ensemble
}
\description{
\code{predict.glue.ann} makes predictions for newdata with
a GLUE-ANN model ensemble
}

