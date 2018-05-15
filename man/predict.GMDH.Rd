\name{predict.GMDH}
\alias{predict.GMDH}
\title{Predicting Using GMDH Algorithm for Binary Classification}
\description{
  This function predicts values based upon a model trained by \code{GMDH}.
}
\usage{
\method{predict}{GMDH}(object, x, type = "class", ...)
}

\arguments{
  \item{object}{an object of class \code{"GMDH"}, created by \code{GMDH}.}
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
  \code{\link{GMDH}}
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

seed <- 11111
set.seed(seed)

nobs <- length(y)

# to split train and test sets
train.indices <- sort(sample(1:nobs, round(nobs*0.8,0)))
test.indices <- (1:nobs)[-train.indices]

x.train <- x[train.indices,]
y.train <- y[train.indices]

x.test <- x[test.indices,]
y.test <- y[test.indices]


set.seed(seed)

# to construct model via GMDH algorithm
model <- GMDH(x.train, y.train)

# to obtain predicted classes for train set
predict(model, x.train, type = "class")
# to obtain predicted probabilities for train set
predict(model, x.train, type = "probability")


# to obtain predicted classes for test set
predict(model, x.test, type = "class")
# to obtain predicted probabilities for test set
predict(model, x.test, type = "probability")

}

\keyword{functions}

