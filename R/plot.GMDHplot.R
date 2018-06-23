plot.GMDHplot <- function(x, ...){
  
  plot(x$plot_list[[1]],x$plot_list[[2]], main = "Performance for Validation Set", xlab = "Layer", ylab = x$plot_list[[3]],  type = "b", xaxt="n")
  abline(h = x$plot_list[[4]], v = x$plot_list[[5]], lty = 2)
  axis(1, at = x$plot_list[[1]])
  
}
