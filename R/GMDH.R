GMDH <- function(x, y, rate = 0.75, alpha = 0.6, maxlayers = 10, maxneurons = 15, exCriterion = "MSE", verbose = TRUE, ...){
  
  
  
  if (!is.matrix(x)) stop("x must be a matrix.")
  if (!is.factor(y)) stop("y must be a factor.")
  if ((rate<=0)|(rate>=1)) stop("rate must be between 0 and 1. Minimum rate is suggested to be 0.5.")
  
  ylevels <- levels(y)
  y <- factor(y, levels = ylevels, labels = 0:1)
  
  if (exCriterion == "MSE")  {outname<- "Mean Square Error"
  }else if (exCriterion == "MAE") {outname<- "Mean Absolute Error"
  }else stop("Correct the external criterion argument.")
  
  
  EXC <- function(data, reference, measure = "MSE") {
    
    
    if(is.factor(reference)) reference <- as.numeric(reference)-1
    
    
    if (measure == "MSE")  {out <- mean((data-reference)^2)
    }else if (measure == "MAE"){out <- mean(abs(data-reference))
    }else stop("Please correct the external criteria.")
    
    return(out)
  }  
  
  
  
  
  ndata <- dim(x)[1]
  nvar<-dim(x)[2]
  
  if (is.null(colnames(x))) cnames <- 1:nvar else cnames <- colnames(x)
  
  
  ntrain <- round(rate*ndata,0)
  nvalidation <- ndata - ntrain
  
  train.indices <- sort(sample(1:ndata, ntrain))
  validation.indices <- (1:ndata)[-train.indices]
  
  x.train <- x[train.indices,]
  y.train <- y[train.indices]
  x.validation <- x[validation.indices,]
  y.validation <- y[validation.indices]
  
  
  
  
  input <- nvar
  nnode = input*(input - 1)/2
  idn = c(1:input)
  w = t(combn(order(idn), 2))
  
  
  store<-NULL
  i=1
  store[[i]]<-list()
  
  
  store[[i]][[1]]<-lapply(1:nnode, function(j) cbind(1, x.train[, w[j, ]]))
  store[[i]][[2]]<-lapply(1:nnode, function(j) ginv(t(store[[i]][[1]][[j]]) %*% store[[i]][[1]][[j]]) %*% t(store[[i]][[1]][[j]]) %*% (as.numeric(y.train)-1))
  store[[i]][[3]]<-lapply(1:nnode, function(j) cbind(1, x.validation[, w[j, ]]))
  store[[i]][[4]]<-lapply(1:nnode, function(j) as.numeric(t(store[[i]][[2]][[j]]) %*% t(store[[i]][[3]][[j]])))
  store[[i]][[6]]<-lapply(1:nnode, function(j) EXC(store[[i]][[4]][[j]], y.validation, measure = exCriterion))
  store[[i]][[13]]<-length(which(unlist(store[[i]][[6]])<(1-alpha)*max(unlist(store[[i]][[6]]))+alpha*min(unlist(store[[i]][[6]]))))
  store[[i]][[14]]<-ifelse(store[[i]][[13]]>maxneurons,maxneurons,store[[i]][[13]]) 
  store[[i]][[7]]<-sort(order(unlist(store[[i]][[6]]), decreasing = FALSE)[1:store[[i]][[14]]])
  store[[i]][[8]]<-order(unlist(store[[i]][[6]]), decreasing = FALSE)[1]
  store[[i]][[9]]<-lapply(1:nnode, function(j) as.numeric(t(store[[i]][[2]][[j]]) %*% t(store[[i]][[1]][[j]])))
  store[[i]][[10]]<-do.call("cbind", lapply(store[[i]][[7]], function(j) store[[i]][[9]][[j]]))
  store[[i]][[11]]<-do.call("cbind", lapply(store[[i]][[7]], function(j) store[[i]][[4]][[j]]))
  store[[i]][[12]]<-do.call("c", lapply(store[[i]][[8]], function(j) store[[i]][[6]][[j]]))
  
  
  if ((store[[i]][[14]]>1)&(maxlayers>1)){
    repeat{
      
      
      i<-i+1 
      
      input <- dim(store[[i-1]][[10]])[2]
      
      nnode = input*(input - 1)/2
      idn = c(1:input)
      w = t(combn(order(idn), 2))
      
      
      store[[i]]<-list()
      
      store[[i]][[1]]<-lapply(1:nnode, function(j) cbind(1, store[[i-1]][[10]][, w[j, ]]))
      store[[i]][[2]]<-lapply(1:nnode, function(j) ginv(t(store[[i]][[1]][[j]]) %*% store[[i]][[1]][[j]]) %*% t(store[[i]][[1]][[j]]) %*% (as.numeric(y.train)-1))
      store[[i]][[3]]<-lapply(1:nnode, function(j) cbind(1, store[[i-1]][[11]][, w[j, ]]))
      store[[i]][[4]]<-lapply(1:nnode, function(j) as.numeric(t(store[[i]][[2]][[j]]) %*% t(store[[i]][[3]][[j]])))
      store[[i]][[6]]<-lapply(1:nnode, function(j) EXC(store[[i]][[4]][[j]], y.validation, measure = exCriterion))
      store[[i]][[13]]<-length(which(unlist(store[[i]][[6]])<(1-alpha)*max(unlist(store[[i]][[6]]))+alpha*min(unlist(store[[i]][[6]]))))
      store[[i]][[14]]<-ifelse(store[[i]][[13]]>maxneurons,maxneurons,store[[i]][[13]]) 
      store[[i]][[7]]<-sort(order(unlist(store[[i]][[6]]), decreasing = FALSE)[1:store[[i]][[14]]])
      store[[i]][[8]]<-order(unlist(store[[i]][[6]]), decreasing = FALSE)[1]
      store[[i]][[9]]<-lapply(1:nnode, function(j) as.numeric(t(store[[i]][[2]][[j]]) %*% t(store[[i]][[1]][[j]])))
      store[[i]][[10]]<-do.call("cbind", lapply(store[[i]][[7]], function(j) store[[i]][[9]][[j]]))
      store[[i]][[11]]<-do.call("cbind", lapply(store[[i]][[7]], function(j) store[[i]][[4]][[j]]))
      store[[i]][[12]]<-do.call("c", lapply(store[[i]][[8]], function(j) store[[i]][[6]][[j]]))
      
      
      if ((store[[i]][[12]]>=store[[i-1]][[12]])|((i-1)==maxlayers)) {
        break
      }else{if (store[[i]][[14]]<=1) break}
      
    }
  }
  
  
  perf<-do.call("c", lapply(c(1:i), function(j) store[[j]][[12]]))
  
  
  if (i==1){
    nlayer<-1
  }else{
    nlayer<-ifelse (!((store[[i]][[12]]>=store[[i-1]][[12]])|((i-1)==maxlayers))&(store[[i]][[14]]<=1),i,i-1)
  }
  
  
plot_list <- list(c(1:i),perf,ylab = outname,h = 1, v = nlayer)
  
  
  
  perf <- perf[1:nlayer]
  sneurons <- c(do.call("c", lapply((1:(nlayer-1)), function(i) store[[i]][[14]])), 1)
  tneurons <- c(nvar*(nvar-1)/2,sneurons[-(nlayer)]*(sneurons[-(nlayer)]-1)/2)
  store_last<-lapply(1:nlayer, function(j) store[[j]])
  
  
  
  ninputs <- c(nvar, sneurons[-nlayer])
  selected <- 1
  for (i in nlayer:1){
    
    if (i==nlayer) selected2<-store[[i]][[8]][selected]
    if (i!=nlayer) selected2<-store[[i]][[7]][selected]
    
    idn = c(1:ninputs[i])
    combinations = t(combn(order(idn), 2))
    selected<-sort(unique(as.numeric(combinations [selected2,])))
  }
  
  
  structure <- as.data.frame(cbind(1:nlayer, "",tneurons,"",sneurons,"", perf))
  colnames(structure) <- c("Layer", "   ","Neurons","   ","Selected neurons","   ", paste("Min",exCriterion))
  
  cnames2 <- data.frame(cnames[sort(selected)])
  colnames(cnames2)<-""
  if (verbose) {
    cat("\n")
    cat(" Structure :", "\n\n", sep = " ")
    print(structure, row.names = FALSE)
    cat("\n")
   
    cat(" External criterion :", outname, "\n\n", sep = " ")
    cat(paste(" Feature selection  :", length(cnames[sort(selected)]), "out of",nvar,"variables are selected."),"\n")
    print(cnames2, row.names = FALSE) 
    cat("\n\n")

}
  
  result <- list()
  result$architecture <- store_last
  result$nlayer <- nlayer
  result$neurons <- tneurons
  result$sneurons <- sneurons
  result$structure <- structure
  result$levels <- ylevels
  result$train.indices <- train.indices
  result$valid.indices <- validation.indices
  result$features <- cnames[sort(selected)]
  result$pfeatures <- sort(selected)
  result$nvar <- nvar
  result$plot_list <- plot_list
  
  attr(result, "class") <- c("GMDH","GMDHplot")
  invisible(result)
  
  
}
