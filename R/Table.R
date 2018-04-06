Table <- function(x, y, option = "min-max", percentages = "column", ndigits = c(2,0), output = NULL) {
  
  if (class(x)!="data.frame") stop("x must be a data.frame.")
  if (class(y)!="factor") stop("y must be a factor.")
  if (all(complete.cases(x))==FALSE) stop("x includes missing observations.") 
  if (all(complete.cases(y))==FALSE) stop("y includes missing observations.") 
  if (dim(x)[1]!=length(y)) stop("The numbers of observations in x and y must be equal.")
  if (length(levels(y))!=2) stop("y must be a factor with two levels.")
  
  nvar <- dim(x)[2]
  
  qtable <- function(mean,sd,median,lower,upper){
    
    pmsign<-" \u00b1 "
    Encoding(pmsign)<-"UTF-8"
    paste(mean,pmsign,sd," (", median, ", ",lower," - ",upper,")",sep = "")
    
  }
  
  pmsign<-" \u00b1 "
  Encoding(pmsign)<-"UTF-8"
  
  ftable <- function(freq, perc){
    
    nrow <- dim(freq)[1]
    ncolumn <- dim(freq)[2]
    
    out<-matrix(NA,nrow,ncolumn)
    
    for (i in 1:nrow){
      for (j in 1:ncolumn){
        out[i,j]<-paste(freq[i,j]," (", perc[i,j],"%)",sep = "")
      }
    }
    
    colnames(out)<-colnames(freq)
    out2<-data.frame(cbind(rownames(freq),out))
    
    return(out2)
    
  }
  y.levels <- levels(factor(y))
  store3 <- NULL
  for (i in 1:nvar){
    store2 <- NULL
    varnames <- colnames(x)
    if ((any(class(x[,i])=="numeric")==TRUE)|(any(class(x[,i])=="integer")==TRUE)){
      
      x.mean <- x.median <- x.firstq <- x.thirdq <- x.min <- x.max <- x.sd <- NULL
      
      for (j in y.levels) {
        store <- NULL
        x.mean <- mean(x[y == j,i])
        x.median <- median(x[y == j,i])
        x.firstq <- as.numeric(quantile(x[y == j,i])[2])
        x.thirdq <- as.numeric(quantile(x[y == j,i])[4])
        x.min <- min(x[y == j,i])
        x.max <- max(x[y == j,i])
        x.sd <- sd(x[y == j,i])
        if (option == "min-max"){
          lower <- x.min
          upper <- x.max
        }else if (option == "Q1-Q3"){
          lower <- x.firstq
          upper <- x.thirdq
        }else stop("Specify option argument.")
        
        store <- qtable(format(round(x.mean,ndigits[1]),nsmall = ndigits[1]),format(round(x.sd,ndigits[1]),nsmall = ndigits[1]),format(round(x.median,ndigits[1]),nsmall = ndigits[1]),format(round(lower,ndigits[1]),nsmall = ndigits[1]),format(round(upper,ndigits[1]),nsmall = ndigits[1]))
        store2 <- data.frame(c(store2,store))
        
      }
      
      store2 <- data.frame(varnames[i], store2)
      
    }else if (any(class(x[,i])=="factor")==TRUE){
      
      freq <- as.data.frame.matrix(table(x[,i],y))
      
      
      if (percentages == "column"){
        percent <- as.data.frame.matrix(t(t(table(x[,i],y))/colSums(table(x[,i],y))))*100
      }else if (percentages == "row"){
        percent <- as.data.frame.matrix(table(x[,i],y)/rowSums(table(x[,i],y)))*100
      }else if (percentages == "total"){
        percent <- as.data.frame.matrix(table(x[,i],y)/sum(table(x[,i],y)))*100
      }else stop("Correct percentages argument.")
      
      store2 <- ftable(format(round(freq,0),nsmall = 0),format(round(percent,ndigits[2]),nsmall = ndigits[2]))
      addvarn <- data.frame(varnames[i],"","")
      colnames(addvarn) <- colnames(store2)
      store2 <- rbind(addvarn,store2)
      
      
    }else stop("The variables must be numeric, integer or factor.")
    
    
    colnames(store2) <- c(" ", y.levels)
    store3 <- rbind(store3, store2)
    
  }
  
  size <- NULL
  y.levels <- levels(factor(y))
  for (i in y.levels){
    size <-cbind(size, length(y[y==i]))
  }
  size <- data.frame(cbind("Observations",size))
  colnames(size) <- c(" ", y.levels)
  store3 <- rbind(size, store3)
  
  if (is.null(output)){
  underline <- data.frame(matrix(rep("---",3),1,3))
  colnames(underline) <- c(" ", y.levels)
  store3 <- rbind(underline,store3)
  }
  
  
  store4 <- data.frame(store3[,1], " ", store3[,2], " ", store3[,3])
  colnames(store4) <- c(" ", "" , y.levels[1]," " ,y.levels[2])
  
  ncharacter <- matrix(NA,dim(store4)[1],dim(store4)[2])
  for (i in 1:dim(store4)[1]){
    
    for (j in 1:dim(store4)[2]){
      
      ncharacter[i,j] <- nchar(toString((store4[i,j])))
      
    }
    
  }
  maxentry <- sum(apply(ncharacter, MARGIN = 2, function(x) max(x, na.rm=TRUE)))
  
  if (is.null(output)){
  
  line<-paste(c("|",rep("=",maxentry+5),"|"),sep = "")
  cat("\n",line,sep = "","\n")
  print(store4,row.names = FALSE)
  cat(line,sep = "","\n\n")
  }else if (output == "latex"){
    
    out <- xtable(store4, align = "rrrrrr")
    print(out,include.rownames = FALSE)
    
  }else if (output == "html"){
    
    out <- xtable(store4, align = "rrrrrr")
    print(out, type = "html", include.rownames = FALSE)
    
  }else stop("Correct output argument.")
  
}


