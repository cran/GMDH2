\name{confMat}
\alias{confMat}
\alias{confMat.default}
\alias{confMat.table}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Confusion Matrix
}
\description{
\code{confMat} constructs a 2\eqn{\times}2 confusion matrix and returns some statistics related to confusion matrix.
}

\usage{
confMat(data, ...)

\method{confMat}{default}(data, reference, positive = NULL, verbose = TRUE, ...)

\method{confMat}{table}(data, positive = NULL, verbose = TRUE, ...)

}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{a factor of predicted classes (for the default method) or an object of class \code{\link[base]{table}}.}
\item{\dots}{option to be passed to \code{table}. Note: do not include \code{reference} here.}
  \item{reference}{a factor of classes to be used as the true results.}
  \item{positive}{an optional character string for the factor level that corresponds to a "positive" result.}
  \item{verbose}{a logical for printing output to R console.}

}

\details{

The \code{confMat} function requires that the factors have exactly the same levels. The function constructs 2\eqn{\times}2 confusion matrix and calculates accuracy, no information rate (NIR), unweighted Kappa statistic, Matthews correlation coefficient, sensitivity, specificity, positive predictive value (PPV), negative predictive value (NPV), prevalence, balanced accuracy, youden index, detection rate, detection prevalence, precision, recall and F1 measure.

Suppose a 2\eqn{\times}2 table with notation

\tabular{rcc}{ \tab Reference \tab \cr Predicted \tab Event \tab No Event
\cr Event \tab TP \tab FP \cr No Event \tab FN \tab TN \cr }

TP is the number of true positives, FP is the number of false positives, FN is the number of false negatives and TN is the number of true negatives.


\deqn{Accuracy = \frac{TP + TN}{TP + FP + FN + TN}} 
\deqn{NIR = max(Prevalence, 1 - Prevalence)}
\deqn{Kappa = \frac{Accuracy - \frac{(TP + FP)(TP + FN)+(FN + TN)(FP + TN)}{(TP + FP + FN + TN)^2}}{1 - \frac{(TP + FP)(TP + FN)+(FN + TN)(FP + TN)}{(TP + FP + FN + TN)^2}}}

\deqn{MCC = \frac{TP \times TN - FN \times FN}{\sqrt{(TP+FP) \times (FN+TN) \times (TP+FN) \times (FP+TN)}} }

\deqn{Sensitivity = \frac{TP}{TP+FN}}
\deqn{Specificity = \frac{TN}{TN+FP}}
\deqn{PPV = \frac{TP}{TP+FP}}
\deqn{NPV = \frac{TN}{TN+FN}}
\deqn{Prevalence = \frac{TP + FN}{TP + FP + FN + TN}}
\deqn{Balanced\ accuracy = \frac{Sensitivity + Specificity}{2}}
\deqn{Youden\ index = Sensitivity + Specificity -1}
\deqn{Detection\ rate = \frac{TP}{TP + FP + FN + TN}}
\deqn{Detection\ prevalence = \frac{TP+FP}{TP + FP + FN + TN}}
\deqn{Precision = \frac{TP}{TP + FP}}
\deqn{Recall = \frac{TP}{TP+FN}}
\deqn{F1 = \frac{2}{\frac{1}{Recall}+\frac{1}{Precision}}}

}
\note{
If the factors, reference and data, have the same levels, but in the incorrect order, the \code{confMat} will reorder data with the order of reference.
}


\value{
Returns a list containing following elements:
\item{table}{confusion matrix}
\item{accuracy}{accuracy}
\item{NIR}{no information rate}
\item{kappa}{unweighted kappa}
\item{MCC}{Matthews correlation coefficient}
\item{sensitivity}{sensitivity}
\item{specificity}{specificity}
\item{PPV}{positive predictive value}
\item{NPV}{negative predictive value}
\item{prevalence}{prevalence}
\item{baccuracy}{balanced accuracy}
\item{youden}{youden index}
\item{detectRate}{detection rate}
\item{detectPrev}{detection prevalence}
\item{precision}{precision}
\item{recall}{recall}
\item{F1}{F1 measure}
\item{all}{returns a matrix containing all statistics}

}

\author{
Osman Dag
}

\seealso{
\code{\link[caret]{confusionMatrix}} 
 
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

# to obtain statistics from table
result <- table(y.test_pred, y.test)
confMat(result, positive = "malignant")


}



\keyword{functions}
