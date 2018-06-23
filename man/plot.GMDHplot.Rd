\name{plot.GMDHplot}
\alias{plot.GMDHplot}
\title{Plotting Using GMDH and dce-GMDH Algorithms for Binary Classification}
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
# to construct model via GMDH algorithm
model <- GMDH(x.train, y.train, x.valid, y.valid)
plot(model)

# to construct model via dce-GMDH algorithm
model2 <- dceGMDH(x.train, y.train, x.valid, y.valid)
plot(model2)


}

\keyword{functions}

