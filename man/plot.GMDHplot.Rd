\name{plot.GMDHplot}
\alias{plot.GMDHplot}
\title{Plotting Using GMDH and dce-GMDH Algorithm for Binary Classification}
\description{
  This function plots minimum specified external criterion across layers based upon a model trained by \code{GMDH} or \code{dceGMDH}. This is plotted for validation set.
}
\usage{
\method{plot}{GMDHplot}(x, ...)
}

\arguments{
  \item{x}{an object of class created by \code{GMDH} or \code{dceGMDH}.}
  \item{\dots}{currently not used.}
}

\author{
Osman Dag
}

\seealso{
  \code{\link{GMDH}},  \code{\link{dceGMDH}}

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
plot(model)

# to construct model via dce-GMDH algorithm
model2 <- dceGMDH(x.train, y.train)
plot(model2)


}

\keyword{functions}

