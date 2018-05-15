\name{Table}
\alias{Table}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Table for Simple Descriptive Statistics}
\description{
\code{Table} produces a table for simple descriptive statistics for a binary response.
} 

\usage{Table(x, y, option = "min-max", percentages = "column", ndigits = c(2,1), 
  output = "R")}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a data frame including all variables.}
  \item{y}{a factor of binary response variable.} 
  \item{option}{an option to return "min-max" or "Q1-Q3". Default is set to "min-max".}
  \item{percentages}{a character string to select the desired percentages. To use column or row percentages, percentages should be set to "column" or "row", respectively. The percentages argument is set to "total" to obtain total percentages. Default is set to "column".} 
  \item{ndigits}{a vector of two numbers. The first one is used to specify the number of digit for numeric/integer variables. The second one specifies the number of digits for percentages of factor/ordered variables. Default is set to ndigits = c(2,1).} 
  \item{output}{a character string to specify the format of descriptive statistics. If output = "LaTeX", it returns the table as latex format. If output = "HTML", it produces the table as html format. If output = "R", it returns the table in R console.} 

}


\author{
Osman Dag
}



\examples{

library(GMDH2)

library(mlbench)
data(BreastCancer)

data <- BreastCancer

# to obtain complete observations
completeObs <- complete.cases(data)
data <- data[completeObs,]

x <- data[,2:10]
y <- data[,11]

Table(x, y)

}

\keyword{functions}
