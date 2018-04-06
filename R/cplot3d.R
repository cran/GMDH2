cplot3d <- function(x1, x2, x3, ypred, yobs, colors = c("red", "blue"), symbols = c("circle","o"), size = 10, xlab = NULL, ylab = NULL, zlab = NULL, title = NULL){
  
  
  
  if (length(colors)!=2) stop("Two colors must be specified.")
  if (length(symbols)!=2) stop("Two symbols must be specified.")
  if (!is.factor(yobs)) stop("The observed binary response variable must be a factor.") 
  if (!is.factor(ypred)) stop("The predicted binary response variable must be a factor.") 
  if (!is.numeric(x1)) stop("The variable on x axis must be a numeric variable.") 
  if (!is.numeric(x2)) stop("The variable on y axis must be a numeric variable.") 
  if (!is.numeric(x3)) stop("The variable on z axis must be a numeric variable.") 
  
  nobs <- c(length(x1),length(x2), length(x3), length(yobs),length(ypred))
  if(length(unique(nobs)) != 1) stop("The length of all variables must be equal.")
  
  
  ifelse(is.null(xlab), dname1<-deparse(substitute(x1)), dname1<-xlab)
  ifelse(is.null(ylab), dname2<-deparse(substitute(x2)), dname2<-ylab)
  ifelse(is.null(zlab), dname3<-deparse(substitute(x3)), dname3<-zlab)
  
  
  classification <- yobs==ypred
  
  data <- data.frame(x1,x2,x3,yobs,classification)
  
  p<-plot_ly(data = data, x = ~x1, y = ~x2, z = ~x3, symbol = ~yobs, symbols = symbols,
             color = ~classification, colors = colors, marker = list(size = size)) %>%
    add_markers() %>%

  
  layout(title = title, scene = list(
    xaxis = list(title = dname1),
    yaxis = list(title = dname2),
    zaxis = list(title = dname3)))
  
  options(warn=-1)
  return(p)
}
