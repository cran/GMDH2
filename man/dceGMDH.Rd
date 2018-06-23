\name{dceGMDH}
\alias{dceGMDH}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Diverse Classifiers Ensemble Based on GMDH-Type Neural Network Algorithm for Binary Classification
}
\description{
\code{dceGMDH} makes a binary classification via diverse classifiers ensemble Based on GMDH-Type Neural Network (dce-GMDH) Algorithm.
}

\usage{dceGMDH(x.train, y.train, x.valid, y.valid, alpha = 0.6, maxlayers = 10, 
  maxneurons = 15, exCriterion = "MSE", verbose = TRUE, svm_options, 
   randomForest_options, naiveBayes_options, cv.glmnet_options, nnet_options, ...)}

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
  \item{svm_options}{a list for options of \code{\link[e1071]{svm}}.}
  \item{randomForest_options}{a list for options of \code{\link[randomForest]{randomForest}}.}
  \item{naiveBayes_options}{a list for options of \code{\link[e1071]{naiveBayes}}.}
  \item{cv.glmnet_options}{a list for options of \code{\link[glmnet]{cv.glmnet}}.}
  \item{nnet_options}{a list for options of \code{\link[nnet]{nnet}}.}
  \item{...}{not used currently.} 

}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}


\value{
A list with class "dceGMDH" and "GMDHplot" containing the following components:

\item{architecture}{all objects stored in construction  process of network}
\item{nlayer}{the number of layers}
\item{neurons}{the number of neurons in layers}
\item{sneurons}{the number of selected neurons in layers}
\item{structure}{the summary structure of the process}
\item{levels}{the levels of binary response}
\item{base_perf}{the performances of the classifiers on validation set at base training}
\item{base_models}{the constructed base classifiers models}
\item{classifiers}{the names of assembled classifiers}
\item{plot_list}{the list of objects to be used in \code{plot.GMDHplot}}

}

\references{

Dag, O., Yozgatligil, C. (2016). GMDH: An R Package for Short Term Forecasting via GMDH-Type Neural Network Algorithms. \emph{The R Journal}, \bold{8:1}, 379-386.

Ivakhnenko, A. G. (1966). Group Method of Data Handling - A Rival of the Method of Stochastic Approximation. \emph{Soviet Automatic Control}, \bold{13}, 43-71.

Kondo, T., Ueno, J. (2006). Revised GMDH-Type Neural Network Algorithm With A Feedback Loop Identifying Sigmoid Function Neural Network. \emph{International Journal of Innovative Computing, Information and Control}, \bold{2:5}, 985-996. 
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
# to construct model via dce-GMDH algorithm
model <- dceGMDH(x.train, y.train, x.valid, y.valid)

# to obtain predicted classes for test set
predict(model, x.test)



}

\keyword{functions}
