\name{GMDH}
\alias{GMDH}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Classification and Feature Selection via GMDH-Type Neural Network Algorithm for a Binary Response
}
\description{
\code{GMDH} makes classification and feature selection via GMDH-type neural network algorithm.
}

\usage{GMDH(x, y, rate = 0.75, alpha = 0.6, maxlayers = 10, maxneurons = 15, 
  exCriterion = "MSE", plot = TRUE, verbose = TRUE, ...)}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a nxp matrix, n is the number of observations and p is the number of variables.}
  \item{y}{a factor of binary response variable.}
  \item{rate}{the rate of observations to be included in train set. The rest of the observations is used for validation. Defaults rate = 0.75.} 
  \item{alpha}{the selection pressure in a layer.  Defaults alpha = 0.6.} 
  \item{maxlayers}{the number of maximum layers. Defaults maxlayers = 10.} 
  \item{maxneurons}{the number of maximum neurons selected in each layer. Defaults maxneurons = 15.} 
  \item{exCriterion}{a character string to select an external criteria. "MSE": Mean Square Error, "MAE": Mean Absolute Error, "MAPE": Mean Absolute Percentage Error. Default is set to "MSE".} 
  \item{plot}{a logical to plot the minimum external criteria in layers. Defaults plot = TRUE.}
  \item{verbose}{a logical for printing summary output to R console.}
  \item{...}{not used currently.} 

}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}


\value{
A list with class "GMDH" containing the following components:

\item{architecture}{all objects stored in construction  process of network}
\item{nlayer}{the number of layers}
\item{neurons}{the number of neurons in layers}
\item{sneurons}{the number of selected neurons in layers}
\item{structure}{the summary structure of the process}
\item{levels}{the levels of binary response}
\item{train.indices}{the indices of training set}
\item{valid.indices}{the indices of validation set}
\item{features}{the names of variables used in GMDH algorithm}
\item{pfeatures}{the column number of variables used in GMDH algorithm}
\item{nvar}{the number of variables in the data set}


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

# to construct model via GMDH algorithm
model <- GMDH(x.train, y.train)


}

\keyword{functions}
