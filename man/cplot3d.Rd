\name{cplot3d}
\alias{cplot3d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
3D Scatter Plot with Classification Labels
}
\description{
\code{cplot3d} produces three dimensional scatter plot with classification labels of binary classes.
} 

\usage{cplot3d(x1, x2, x3, ypred, yobs, colors = c("red", "blue"), 
  symbols = c("circle", "o"), size = 10, xlab = NULL, ylab = NULL, zlab = NULL, 
  title = NULL)}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x1}{the x coordinate of points in the plot.}
  \item{x2}{the y coordinate of points in the plot.} 
  \item{x3}{the z coordinate of points in the plot.} 
  \item{ypred}{a factor of the predicted binary response variable.}
  \item{yobs}{a factor of the observed binary response variable.} 
  \item{colors}{a vector of two colors specifying the levels of the observed binary response variable.} 
  \item{symbols}{a vector of two symbols specifying the levels of the predicted binary response variable.} 
  \item{size}{the size of symbols.}
  \item{xlab}{a label for the x axis, defaults to a description of x1.} 
  \item{ylab}{a label for the y axis, defaults to a description of x2.} 
  \item{zlab}{a label for the z axis, defaults to a description of x3.} 
  \item{title}{a main title of the plot.} 

}

\details{

Symbols indicate the observed classes of binary response. Colors show TRUE or FALSE classification of the observations. 

}


\value{
An object with class "plotly" and "htmlwidget".
}



\author{
Osman Dag
}

\seealso{
\code{\link[plotly]{plot_ly}} 
 
}


\examples{

library(GMDH2)

library(mlbench)
data(BreastCancer)

data <- BreastCancer

# to obtain complete observations
completeObs <- complete.cases(data)
data <- data[completeObs,]

xdata <- data.matrix(data[,2:10])
ydata <- data[,11]

seed <- 10
set.seed(seed)

no.rows <- length(ydata)

# to split train and test sets
train.indices <- sort(sample(1:no.rows, round(no.rows*0.8,0)))
test.indices <- (1:no.rows)[-train.indices]

x.train <- xdata[train.indices,]
y.train <- ydata[train.indices]

x.test <- xdata[test.indices,]
y.test <- ydata[test.indices]


set.seed(seed)

# to construct model via dce-GMDH algorithm
model <- dceGMDH(x.train, y.train)

# to obtain predicted classes for train set
y.train_pred <- predict(model, x.train, type = "class")

# to obtain confusion matrix and some statistics for train set
confMat(y.train_pred, y.train, positive = "malignant")

# to obtain predicted classes for test set
y.test_pred <- predict(model, x.test, type = "class")

# to obtain confusion matrix and some statistics for test set
confMat(y.test_pred, y.test, positive = "malignant")

# to produce 3D scatter plot with classification labels for test set
cplot3d(x.test[,1], x.test[,6], x.test[,3], y.test_pred, y.test,
colors = c("red", "black"))

}

\keyword{functions}
