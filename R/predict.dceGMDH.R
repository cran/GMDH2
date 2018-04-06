predict.dceGMDH <- function(object, x, type = "class", ...){
  
  
  result <- attr(predict(object$base_models[[1]], x, decision.values = F, probability = TRUE), "probabilities")
  ypred_svm <- as.numeric(result[, colnames(result) == "1"])
  
  result <- predict(object$base_models[[2]], x, type = "prob")
  ypred_randomForest <- as.numeric(result[, colnames(result) == "1"])
  
  result <- predict(object$base_models[[3]], x, type = "raw")
  ypred_naiveBayes <- as.numeric(result[, colnames(result) == "1"])
  
  result <- predict(object$base_models[[4]], x, s = "lambda.min", type = "response")
  ypred_cv.glmnet <- result[, 1]
  
  result <- predict(object$base_models[[5]], x, type = "raw")
  ypred_nnet <- as.numeric(result[, colnames(result) == "1"])
  
  ypredd <- cbind(ypred_svm,ypred_randomForest,ypred_naiveBayes,ypred_cv.glmnet,ypred_nnet)
  
  
  store<-NULL
  
  if (object$nlayer==0){
    prob <- ypredd[,which.min(object$base_perf)]
  }else{
    
    i=1
    store[[i]]<-list()  
    x <- ypredd
    idn = c(1:object$sneurons[i])
    combinations = t(combn(order(idn), 2))
    
    
    
    store[[i]][[1]]<-lapply(1:object$neurons[i+1], function(j) cbind(1, x[, combinations[j, ]]))
    store[[i]][[2]]<-lapply(1:object$neurons[i+1], function(j) as.numeric(t(object$architecture[[i]][[2]][[j]]) %*% t(store[[i]][[1]][[j]])))
    store[[i]][[3]]<-do.call("cbind", lapply(object$architecture[[i]][[7]], function(j) store[[i]][[2]][[j]]))
    
    
    if (i != object$nlayer){
      repeat{
        i<-i+1 
        
        store[[i]]<-list()
        
        idn = c(1:object$sneurons[i])
        combinations = t(combn(order(idn), 2))
        
        store[[i]][[1]]<-lapply(1:object$neurons[i+1], function(j) cbind(1, store[[i-1]][[3]][, combinations[j, ]]))
        store[[i]][[2]]<-lapply(1:object$neurons[i+1], function(j) as.numeric(t(object$architecture[[i]][[2]][[j]]) %*% t(store[[i]][[1]][[j]])))
        store[[i]][[3]]<-do.call("cbind", lapply(object$architecture[[i]][[7]], function(j) store[[i]][[2]][[j]]))
        
        
        if (i == object$nlayer) break 
      }
    }
    
    prob <- as.numeric(store[[object$nlayer]][[2]][[object$architecture[[i]][[8]]]])
    
  }
  prob2<-ifelse(prob<0, 0, ifelse(prob>1, 1, prob)) 
  
  
  if (type == "probability"){
    out<-cbind(1-prob2,prob2)
    colnames(out) <- object$levels
  }else if (type == "class"){
    out <- factor(ifelse(prob2>=0.5,object$levels[2],object$levels[1]))
  }else {stop("Please correct type option.")}
  
  return(out)
  
}

