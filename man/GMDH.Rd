\name{GMDH}
\alias{GMDH}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Feature Selection and Classification via GMDH-Type Neural Network Algorithm for a Binary Response
}
\description{
\code{GMDH} makes feature selection and classification via GMDH-type neural network algorithm.
}

\usage{GMDH(x.train, y.train, x.valid, y.valid, alpha = 0.6, maxlayers = 10, maxneurons = 15, 
  exCriterion = "MSE", verbose = TRUE, ...)}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x.train}{a n1xp matrix to be included in model construction, n1 is the number of observations and p is the number of variables.}
  \item{y.train}{a factor of binary response variable to be included in model construction.}
  \item{x.valid}{a n2xp matrix to be used for neuron selection, n2 is the number of observations and p is the number of variables.}
  \item{y.valid}{a factor of binary response variable to be used for neuron selection.}
  \item{alpha}{the selection pressure in a layer.  Defaults alpha = 0.6.} 
  \item{maxlayers}{the number of maximum layers. Defaults maxlayers = 10.} 
  \item{maxneurons}{the number of maximum neurons selected in each layer. Defaults maxneurons = 15.} 
  \item{exCriterion}{a character string to select an external criteria. "MSE": Mean Square Error, "MAE": Mean Absolute Error. Default is set to "MSE".} 
  \item{verbose}{a logical for printing summary output to R console.}
  \item{...}{not used currently.} 

}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}


\value{
A list with class "GMDH" and "GMDHplot" containing the following components:

\item{architecture}{all objects stored in construction  process of network}
\item{nlayer}{the number of layers}
\item{neurons}{the number of neurons in layers}
\item{sneurons}{the number of selected neurons in layers}
\item{structure}{the summary structure of the process}
\item{levels}{the levels of binary response}
\item{features}{the names of variables used in GMDH algorithm}
\item{pfeatures}{the column number of variables used in GMDH algorithm}
\item{nvar}{the number of variables in the data set}
\item{plot_list}{the list of objects to be used in \code{plot.GMDHplot}}

}

\references{

Dag, O., Karabulut, E., Alpar, R. (2019). GMDH2: Binary Classification via GMDH-Type Neural Network Algorithms - R Package and Web-Based Tool. \emph{International Journal of Computational Intelligence Systems}, \bold{12:2}, 649-660.

}

\author{
Osman Dag, Erdem Karabulut, Reha Alpar
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
predict(model, x.test)

}

\keyword{functions}
