predict.GMDH <- function(object, x, type = "class", ...){
  

  store<-NULL

    i=1
    store[[i]]<-list()  
    idn = 1:object$nvar
    combinations = t(combn(order(idn), 2))
    
    
    
    store[[i]][[1]]<-lapply(1:object$neurons[i], function(j) cbind(1, x[, combinations[j, ]]))
    store[[i]][[2]]<-lapply(1:object$neurons[i], function(j) as.numeric(t(object$architecture[[i]][[2]][[j]]) %*% t(store[[i]][[1]][[j]])))
    store[[i]][[3]]<-do.call("cbind", lapply(object$architecture[[i]][[7]], function(j) store[[i]][[2]][[j]]))
    
    
    if (i != object$nlayer){
      repeat{
        i<-i+1 
        
        store[[i]]<-list()
        
        idn = c(1:object$sneurons[i-1])
        combinations = t(combn(order(idn), 2))
        
        store[[i]][[1]]<-lapply(1:object$neurons[i], function(j) cbind(1, store[[i-1]][[3]][, combinations[j, ]]))
        store[[i]][[2]]<-lapply(1:object$neurons[i], function(j) as.numeric(t(object$architecture[[i]][[2]][[j]]) %*% t(store[[i]][[1]][[j]])))
        store[[i]][[3]]<-do.call("cbind", lapply(object$architecture[[i]][[7]], function(j) store[[i]][[2]][[j]]))
        
        
        if (i == object$nlayer) break 
      }
    }
    
    prob <- as.numeric(store[[object$nlayer]][[2]][[object$architecture[[i]][[8]]]])
    
  
  prob2<-ifelse(prob<0, 0, ifelse(prob>1, 1, prob)) 
  
  
  if (type == "probability"){
    out<-cbind(1-prob2,prob2)
    colnames(out) <- object$levels
  }else if (type == "class"){
    out <- factor(ifelse(prob2>=0.5,object$levels[2],object$levels[1]))
  }else {stop("Please correct type option.")}
  
  return(out)
  
}
