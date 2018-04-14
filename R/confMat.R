confMat<- function(data, ...) UseMethod("confMat")



confMat.default<-function(data, reference, positive = NULL, verbose = TRUE, ...) {
  

  if(!is.factor(data)) data <- factor(data)
  if(!is.factor(reference)) reference <- factor(reference)
  
  
  if(length(levels(reference))!=2)
    stop("The reference must have 2 factor levels.")
  
  if(any(levels(reference) != levels(data))) {
    warning("Levels are not in the same order for reference and data. Refactoring data to match.")
    data <- as.character(data)
    data <- factor(data, levels = levels(reference))
  }
  


  store <- table(data,reference)
  
  if (!is.null(positive)){
  if (which(colnames(store)==positive)==2) {

temp <- store[1,]
store[1,] <- store [2,]
store[2,] <- temp
temp <- store[,1]
store[,1] <- store[,2]
store[,2] <- temp
temp <- colnames(store)[1]
colnames(store)[1] <- colnames(store)[2]
colnames(store)[2] <- temp
rownames(store) <- colnames(store)

  }}else{positive <- colnames(store)[1]}
    
  
  accuracy <- sum(diag(store))/sum(store)
  
  pc <- sum(diag(rowSums(store) %*% t(colSums(store))))/(sum(store)^2)
  
  kappa <- (accuracy-pc)/(1-pc) 
  
  
  denom <-colSums(store)[1]*colSums(store)[2]*rowSums(store)[1]*rowSums(store)[2]
if(denom==0) denom<-1
  MCC <- (store[1,1]*store[2,2]-store[1,2]*store[2,1])/sqrt(denom)

  ccr <- diag(store)/colSums(store)
  
  sensitivity <- ccr[1]
  specificity <- ccr[2]
  
  pv <- diag(store)/rowSums(store)
  prevalences <- colSums(store)/sum(store)
  NIR <- max(prevalences)
  prevalence <- prevalences[1]
  PPV <- pv[1]
  NPV <- pv[2]
  baccuracy <- (sensitivity+specificity)/2
  youden <- sensitivity+specificity-1
  detectRate <- store[1,1]/sum(store)
  detectPrevalence <- rowSums(store)[1]/sum(store)
  precision <- PPV
  recall <- sensitivity
  F1 <- 2/(1/recall+1/precision)
    
  if (verbose) {
    cat("\n")
    cat("Confusion Matrix and Statistics", "\n\n", sep = " ")
    print(store)
    cat("\n")
    cat("\n", "    Accuracy             :  ", round(accuracy,4), sep = " ")
    cat("\n", "    No Information Rate  :  ", round(NIR,4), sep = " ")
    cat("\n", "    Kappa                :  ", round(kappa,4), sep = " ")
    cat("\n", "    Matthews Corr Coef   :  ", round(MCC,4), sep = " ")
    cat("\n", "    Sensitivity          :  ", round(sensitivity,4), sep = " ")
    cat("\n", "    Specificity          :  ", round(specificity,4), sep = " ")
    cat("\n", "    Positive Pred Value  :  ", round(PPV,4), sep = " ")
    cat("\n", "    Negative Pred Value  :  ", round(NPV,4), sep = " ")
    cat("\n", "    Prevalence           :  ", round(prevalence,4), sep = " ")
    cat("\n", "    Balanced Accuracy    :  ", round(baccuracy,4), sep = " ")
    cat("\n", "    Youden Index         :  ", round(youden,4), sep = " ")
    cat("\n", "    Detection Rate       :  ", round(detectRate,4), sep = " ")
    cat("\n", "    Detection Prevalence :  ", round(detectPrevalence,4), sep = " ")
    cat("\n", "    Precision            :  ", round(precision,4), sep = " ")
    cat("\n", "    Recall               :  ", round(recall,4), sep = " ")
    cat("\n", "    F1                   :  ", round(F1,4), "\n", sep = " ")
    cat("\n", "    Positive Class       :  ", positive, "\n", sep = " ")
    cat("\n")
  }   
  

  all<-rbind(as.numeric(accuracy),as.numeric(NIR),as.numeric(kappa),as.numeric(MCC),as.numeric(sensitivity),as.numeric(specificity),as.numeric(PPV),as.numeric(NPV),as.numeric(prevalence),as.numeric(baccuracy),as.numeric(youden),as.numeric(detectRate),as.numeric(detectPrevalence),as.numeric(precision),as.numeric(recall),as.numeric(F1))


  rownames(all)<-c("accuracy","NIR","kappa","MCC","sensitivity","specificity","PPV","NPV","prevalence","baccuracy","youden","detectRate","detectPrev","precision","recall","F1")
colnames(all)<-""

  
  result <- list()
  result$table <- store
  result$accuracy <- as.numeric(accuracy)
  result$NIR <- as.numeric(NIR)
  result$kappa <- as.numeric(kappa)
  result$MCC <- as.numeric(MCC)
  result$sensitivity <- as.numeric(sensitivity)
  result$specificity <- as.numeric(specificity)
  result$PPV <- as.numeric(PPV)
  result$NPV <- as.numeric(NPV)
  result$prevalence <- as.numeric(prevalence)
  result$baccuracy <- as.numeric(baccuracy)
  result$youden <- as.numeric(youden)
  result$detectRate <- as.numeric(detectRate)
  result$detectPrevalence <- as.numeric(detectPrevalence)
  result$precision <- as.numeric(precision)
  result$recall <- as.numeric(recall)
  result$F1 <- as.numeric(F1)
  result$all <- all
  
  invisible(result)
  
}



confMat.table<-function(data, positive = NULL, verbose = TRUE, ...) {
  

  if(length(dim(data)) != 2) stop("The table must have two dimensions.")
  if(nrow(data) != ncol(data)) stop("The number of rows must be equal to the number of columns.")
  if(length(colnames(data))!=2) stop("The reference must have 2 factor levels.")

  store <- data
  
  if (!is.null(positive)){
  if (which(colnames(store)==positive)==2) {

temp <- store[1,]
store[1,] <- store [2,]
store[2,] <- temp
temp <- store[,1]
store[,1] <- store[,2]
store[,2] <- temp
temp <- colnames(store)[1]
colnames(store)[1] <- colnames(store)[2]
colnames(store)[2] <- temp
rownames(store) <- colnames(store)


  }}else{positive <- colnames(store)[1]}
    
  
  accuracy <- sum(diag(store))/sum(store)
  
  pc <- sum(diag(rowSums(store) %*% t(colSums(store))))/(sum(store)^2)
  
  kappa <- (accuracy-pc)/(1-pc) 
  
  denom <-colSums(store)[1]*colSums(store)[2]*rowSums(store)[1]*rowSums(store)[2]
if(denom==0) denom<-1
  MCC <- (store[1,1]*store[2,2]-store[1,2]*store[2,1])/sqrt(denom)

  ccr <- diag(store)/colSums(store)
  
  sensitivity <- ccr[1]
  specificity <- ccr[2]
  
  pv <- diag(store)/rowSums(store)
  prevalences <- colSums(store)/sum(store)
  NIR <- max(prevalences)
  prevalence <- prevalences[1]
  PPV <- pv[1]
  NPV <- pv[2]
  baccuracy <- (sensitivity+specificity)/2
  youden <- sensitivity+specificity-1
  detectRate <- store[1,1]/sum(store)
  detectPrevalence <- rowSums(store)[1]/sum(store)
  precision <- PPV
  recall <- sensitivity
  F1 <- 2/(1/recall+1/precision)
  
  
  if (verbose) {
    cat("\n")
    cat("Confusion Matrix and Statistics", "\n\n", sep = " ")
    print(store)
    cat("\n")
    cat("\n", "    Accuracy             :  ", round(accuracy,4), sep = " ")
    cat("\n", "    No Information Rate  :  ", round(NIR,4), sep = " ")
    cat("\n", "    Kappa                :  ", round(kappa,4), sep = " ")
    cat("\n", "    Matthews Corr Coef   :  ", round(MCC,4), sep = " ")
    cat("\n", "    Sensitivity          :  ", round(sensitivity,4), sep = " ")
    cat("\n", "    Specificity          :  ", round(specificity,4), sep = " ")
    cat("\n", "    Positive Pred Value  :  ", round(PPV,4), sep = " ")
    cat("\n", "    Negative Pred Value  :  ", round(NPV,4), sep = " ")
    cat("\n", "    Prevalence           :  ", round(prevalence,4), sep = " ")
    cat("\n", "    Balanced Accuracy    :  ", round(baccuracy,4), sep = " ")
    cat("\n", "    Youden Index         :  ", round(youden,4), sep = " ")
    cat("\n", "    Detection Rate       :  ", round(detectRate,4), sep = " ")
    cat("\n", "    Detection Prevalence :  ", round(detectPrevalence,4), sep = " ")
    cat("\n", "    Precision            :  ", round(precision,4), sep = " ")
    cat("\n", "    Recall               :  ", round(recall,4), sep = " ")
    cat("\n", "    F1                   :  ", round(F1,4), "\n", sep = " ")
    cat("\n", "    Positive Class       :  ", positive, "\n", sep = " ")
    cat("\n")
  }   
  
  all<-rbind(as.numeric(accuracy),as.numeric(NIR),as.numeric(kappa),as.numeric(MCC),as.numeric(sensitivity),as.numeric(specificity),as.numeric(PPV),as.numeric(NPV),as.numeric(prevalence),as.numeric(baccuracy),as.numeric(youden),as.numeric(detectRate),as.numeric(detectPrevalence),as.numeric(precision),as.numeric(recall),as.numeric(F1))

rownames(all)<-c("accuracy","NIR","kappa","MCC","sensitivity","specificity","PPV","NPV","prevalence","baccuracy","youden","detectRate","detectPrev","precision","recall","F1")
colnames(all)<-""
  
  result <- list()
  result$table <- store
  result$accuracy <- as.numeric(accuracy)
  result$NIR <- as.numeric(NIR)
  result$kappa <- as.numeric(kappa)
  result$MCC <- as.numeric(MCC)
  result$sensitivity <- as.numeric(sensitivity)
  result$specificity <- as.numeric(specificity)
  result$PPV <- as.numeric(PPV)
  result$NPV <- as.numeric(NPV)
  result$prevalence <- as.numeric(prevalence)
  result$baccuracy <- as.numeric(baccuracy)
  result$youden <- as.numeric(youden)
  result$detectRate <- as.numeric(detectRate)
  result$detectPrev <- as.numeric(detectPrevalence)
  result$precision <- as.numeric(precision)
  result$recall <- as.numeric(recall)
  result$F1 <- as.numeric(F1)
  result$all <- all

  invisible(result)


}
