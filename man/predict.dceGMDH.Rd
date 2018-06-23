\name{predict.dceGMDH}
\alias{predict.dceGMDH}
\title{Predicting Using dce-GMDH Algorithm for Binary Classification}
\description{
  This function predicts values based upon a model trained by \code{dceGMDH}.
}
\usage{
\method{predict}{dceGMDH}(object, x, type = "class", ...)
}

\arguments{
  \item{object}{an object of class \code{"dceGMDH"}, created by \code{dceGMDH}.}
  \item{x}{a matrix containing the new input data.}
  \item{type}{a character string to return predicted output. If type = "class", the function returns the predicted classes. If type = "probability", it returns the predicted probabilities. Default is set to "class".}
  \item{\dots}{currently not used.}
}
\value{
  A vector of predicted values of corresponding classes depending on type specified. 
}
\author{
Osman Dag, Erdem Karabulut, Reha Alpar
}
\seealso{
  \code{\link{dceGMDH}}
}
\examples{

library(GMDH2)

library(mlbench)
data(BreastCancer)

data <- BreastCancer

# to obtain complete observations
completeObs <- complete.cases(data)
data <- data[completeObs,]

x <- data.matrix(data[,2:10])
y <- data[,11]

seed <- 12345
set.seed(seed)
nobs <- length(y)

# to split train, validation and test sets

indices <- sample(1:nobs)

ntrain <- round(nobs*0.6,0)
nvalid <- round(nobs*0.2,0)
ntest <- nobs-(ntrain+nvalid)

train.indices <- sort(indices[1:ntrain])
valid.indices <- sort(indices[(ntrain+1):(ntrain+nvalid)])
test.indices <- sort(indices[(ntrain+nvalid+1):nobs])


x.train <- x[train.indices,]
y.train <- y[train.indices]

x.valid <- x[valid.indices,]
y.valid <- y[valid.indices]

x.test <- x[test.indices,]
y.test <- y[test.indices]

set.seed(seed)
# to construct model via dce-GMDH algorithm
model <- dceGMDH(x.train, y.train, x.valid, y.valid)

# to obtain predicted classes for test set
predict(model, x.test, type = "class")
# to obtain predicted probabilities for test set
predict(model, x.test, type = "probability")

}

\keyword{functions}

