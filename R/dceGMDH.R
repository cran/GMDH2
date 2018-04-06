dceGMDH <- function(x, y, rate = 0.75, alpha = 0.6, maxlayers = 5, maxneurons = 15, exCriteria = "MSE", plot = TRUE, verbose = TRUE, svm_options, randomForest_options, naiveBayes_options, cv.glmnet_options, nnet_options, ...){
  
  
  call <- match.call()
  
  if (!is.matrix(x)) stop("x must be a matrix.")
  if (!is.factor(y)) stop("y must be a factor.")
  if ((rate<=0)|(rate>=1)) stop("rate must be between 0 and 1. Minimum rate is suggested to be 0.5.")
  
  ylevels <- levels(y)
  y <- factor(y, levels = ylevels, labels = 0:1)
  
  if (exCriteria == "MSE")  {outname<- "Mean Square Error"
}else if (exCriteria == "MAE") {outname<- "Mean Absolute Error"
}else if (exCriteria == "MAPE") {outname<- "Mean Absolute Percentage Error"
}else stop("Correct the external criterion argument.")
  
  
  EXC <- function(data, reference, measure = "MSE") {
    
    
    if(is.factor(reference)) reference <- as.numeric(reference)-1
    
    
    if (measure == "MSE")  {out <- mean((data-reference)^2)
    }else if (measure == "MAE"){out <- mean(abs(data-reference))
}else if (measure == "MAPE"){out <- 100*mean(abs((data-reference)/reference))
}else stop("Please correct the external criteria.")
    
    return(out)
  }  
  
  
  svm_func <- function(x,y,svm_options){
    if (missing(svm_options)){
      out <- svm(x,y, decision.values = F, probability = TRUE)
    } else {
      out <- do.call(svm, c(list(x,y,decision.values = F, probability = TRUE), svm_options))
    }
    result <- attr(predict(out, x, decision.values = F, probability = TRUE), "probabilities")
    return(list(model = out, pred_prob = as.numeric(result[,colnames(result)=="1"])))
  }
  
  randomForest_func <- function(x,y,randomForest_options){
    if (missing(randomForest_options)){
      out <- randomForest(x, y)
    } else {
      out <- do.call(randomForest, c(list(x,y), randomForest_options))
    }
    result <- predict(out, x, type = "prob")
    return(list(model = out, pred_prob = as.numeric(result[,colnames(result)=="1"])))
  }
  
  naiveBayes_func <- function(x,y,naiveBayes_options){
    if (missing(naiveBayes_options)){
      out <- naiveBayes(x, y)
    } else {
      out <- do.call(naiveBayes, c(list(x,y), naiveBayes_options))
    }
    result <- predict(out, x, type = "raw")
    return(list(model = out, pred_prob = as.numeric(result[,colnames(result)=="1"])))
  }
  
  
  cv.glmnet_func <- function(x,y,cv.glmnet_options){
    if (missing(cv.glmnet_options)){
      out <- cv.glmnet(x,y, family = "binomial")
    } else {
      out <- do.call(cv.glmnet, c(list(x,y, family = "binomial"), cv.glmnet_options))
    }
    result <- predict(out, x, s = "lambda.min", type = "response")
    return(list(model = out, pred_prob = as.numeric(result[,1])))
  }
  
  nnet_func <- function(x,y,nnet_options){
    if (missing(nnet_options)){
      out <- nnet(x, class.ind(y), size = dim(x)[2], decay = 5e-4, trace = FALSE)
    } else {
      out <- do.call(nnet, c(list(x, class.ind(y), size = dim(x)[2]), decay = 5e-4, trace = FALSE, nnet_options))
    }
    result <- predict(out, x, type = "raw")
    return(list(model = out, pred_prob = as.numeric(result[,colnames(result)=="1"])))
  }
  
  ndata <- dim(x)[1]
  nvar<-dim(x)[2]
  
  ntrain <- round(rate*ndata,0)
  nvalidation <- ndata - ntrain
  
  train.indices <- sort(sample(1:ndata, ntrain))
  validation.indices <- (1:ndata)[-train.indices]
  
  x.train <- x[train.indices,]
  y.train <- y[train.indices]
  x.validation <- x[validation.indices,]
  y.validation <- y[validation.indices]
  
  
  svm_result <- svm_func(x.train,y.train,svm_options)
  randomForest_result <- randomForest_func(x.train,y.train,randomForest_options)
  naiveBayes_result <- naiveBayes_func(x.train,y.train,naiveBayes_options)
  cv.glmnet_result <- cv.glmnet_func(x.train,y.train,cv.glmnet_options)
  nnet_result <- nnet_func(x.train,y.train,nnet_options)
  

result <- attr(predict(svm_result$model, x.validation, decision.values = F, probability = TRUE), "probabilities")
ypred_svm <- as.numeric(result[,colnames(result)=="1"])

result <- predict(randomForest_result$model, x.validation, type = "prob")
ypred_randomForest <- as.numeric(result[,colnames(result)=="1"])

result <- predict(naiveBayes_result$model, x.validation, type = "raw")
ypred_naiveBayes <- as.numeric(result[,colnames(result)=="1"])

result <- predict(cv.glmnet_result$model, x.validation, s = "lambda.min", type = "response")
ypred_cv.glmnet <- result[,1]

result <- predict(nnet_result$model, x.validation, type = "raw")
ypred_nnet <- as.numeric(result[,colnames(result)=="1"])


y.train_pred <- cbind(svm_result$pred_prob, randomForest_result$pred_prob, naiveBayes_result$pred_prob, cv.glmnet_result$pred_prob, nnet_result$pred_prob)
y.validation_pred <- cbind(ypred_svm, ypred_randomForest, ypred_naiveBayes, ypred_cv.glmnet, ypred_nnet)
base_perf <- c(EXC(ypred_svm,y.validation),EXC(ypred_randomForest,y.validation),
               EXC(ypred_naiveBayes,y.validation), EXC(ypred_cv.glmnet,y.validation),
               EXC(ypred_nnet,y.validation))
base_models <- list(svm_result$model, randomForest_result$model, naiveBayes_result$model, cv.glmnet_result$model, nnet_result$model)

cnames <- c("svm","randomForest","naiveBayes","cv.glmnet","nnet")

input <- 5
nnode = input*(input - 1)/2
idn = c(1:input)
w = t(combn(order(idn), 2))


store<-NULL
i=1
store[[i]]<-list()


store[[i]][[1]]<-lapply(1:nnode, function(j) cbind(1, y.train_pred[, w[j, ]]))
store[[i]][[2]]<-lapply(1:nnode, function(j) ginv(t(store[[i]][[1]][[j]]) %*% store[[i]][[1]][[j]]) %*% t(store[[i]][[1]][[j]]) %*% (as.numeric(y.train)-1))
store[[i]][[3]]<-lapply(1:nnode, function(j) cbind(1, y.validation_pred[, w[j, ]]))
store[[i]][[4]]<-lapply(1:nnode, function(j) as.numeric(t(store[[i]][[2]][[j]]) %*% t(store[[i]][[3]][[j]])))
store[[i]][[6]]<-lapply(1:nnode, function(j) EXC(store[[i]][[4]][[j]], y.validation, measure = exCriteria))
store[[i]][[13]]<-length(which(unlist(store[[i]][[6]])<(1-alpha)*max(unlist(store[[i]][[6]]))+alpha*min(unlist(store[[i]][[6]]))))
store[[i]][[14]]<-ifelse(store[[i]][[13]]>maxneurons,maxneurons,store[[i]][[13]]) 
store[[i]][[7]]<-sort(order(unlist(store[[i]][[6]]), decreasing = FALSE)[1:store[[i]][[14]]])
store[[i]][[8]]<-order(unlist(store[[i]][[6]]), decreasing = FALSE)[1]
store[[i]][[9]]<-lapply(1:nnode, function(j) as.numeric(t(store[[i]][[2]][[j]]) %*% t(store[[i]][[1]][[j]])))
store[[i]][[10]]<-do.call("cbind", lapply(store[[i]][[7]], function(j) store[[i]][[9]][[j]]))
store[[i]][[11]]<-do.call("cbind", lapply(store[[i]][[7]], function(j) store[[i]][[4]][[j]]))
store[[i]][[12]]<-do.call("c", lapply(store[[i]][[8]], function(j) store[[i]][[6]][[j]]))


if ((store[[i]][[12]]<min(base_perf))&(store[[i]][[14]]>1)){
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
    store[[i]][[6]]<-lapply(1:nnode, function(j) EXC(store[[i]][[4]][[j]], y.validation, measure = exCriteria))
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


perf<-c(min(base_perf), do.call("c", lapply(c(1:i), function(j) store[[j]][[12]])))


if (i==1){
nlayer<-ifelse(((store[[i]][[12]]<min(base_perf))&(store[[i]][[14]]<=1)),i,i-1)
}else{
nlayer<-ifelse (!((store[[i]][[12]]>=store[[i-1]][[12]])|((i-1)==maxlayers))&(store[[i]][[14]]<=1),i,i-1)
}


if (plot == TRUE){
  plot(c(0:i),perf, main = "Performance for Validation Set",xlab = "Layer", ylab = outname,  type = "b", xaxt="n")
  abline(h = 0, v = nlayer, lty = 2)
  axis(1, at = c(0:(nlayer+1)))
}


if (nlayer==0) {
  perf<-min(base_perf)
  sneurons <- 1
  tneurons <- 5
  store_last <- NULL
}else if (nlayer==1){
  perf <- c(min(base_perf), store[[1]][[12]])
  sneurons <- c(5, 1)
  tneurons <- c(5, 10)
  store_last<-lapply(1:nlayer, function(j) store[[j]])
}else{
  perf <- perf[1:(nlayer+1)]
  sneurons <- c(5, do.call("c", lapply((1:(nlayer-1)), function(i) store[[i]][[14]])), 1)
  tneurons <- c(5, sneurons[-(nlayer+1)]*(sneurons[-(nlayer+1)]-1)/2)
  store_last<-lapply(1:nlayer, function(j) store[[j]])
}

if (nlayer==0){
  selected <- which.min(base_perf)
}else{
  selected <- 1
  for (i in nlayer:1){
    
    if (i==nlayer) selected2<-store[[i]][[8]][selected]
    if (i!=nlayer) selected2<-store[[i]][[7]][selected]
    
    idn = c(1:sneurons[i])
    combinations = t(combn(order(idn), 2))
    selected<-unique(as.numeric(combinations [selected2,]))
  }
}

structure <- as.data.frame(cbind(0:nlayer, "",tneurons,"",sneurons,"", perf))
colnames(structure) <- c("Layer", "   ","Neurons","   ","Selected neurons","   ", paste("Min",exCriteria))

if (verbose) {
  cat("\n")
  print(call)  
  cat("\n")
  cat("  Structure :", "\n\n", sep = " ")
  print(structure)
  cat("\n")
  cat("  The assembled classifiers :", cnames[sort(selected)])
  cat("\n\n")
  cat("  External criterion        :", outname, "\n\n", sep = " ")}

result <- list()
result$architecture <- store_last
result$nlayer <- nlayer
result$neurons <- tneurons
result$sneurons <- sneurons
result$structure <- structure
result$levels <- ylevels
result$base_perf <- base_perf
result$base_models <- base_models
result$train.indices <- train.indices
result$valid.indices <- validation.indices


attr(result, "class") <- "dceGMDH"
invisible(result)



}