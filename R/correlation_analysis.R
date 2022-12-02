#' @title correlation_analysis
#' @description performs correlation analysis and corrplots for each of the groups in the dataset
#' @param x 
#' @param result 
#' @param data.info 
#' @param title 
#' @param groups 
#' @param T0 
#' @param time0 
#' @param FC 
#' @param scaling 
#' @import corrplot
#' @import RColorBrewer
#' @return
#' @export correlation_analysis
#' @examples
#' correlation_analysis(x)

correlation_analysis <- function(x, result, data.info = c("sample", "subject", "time", "class"),
                                 title = "VIPs_corr", groups = list(), T0 = F, time0 = NULL, FC = F, scaling = T) {
  if (T0 == T) {
    x_T0 <- zero.norm(x, time0 = time0, data.info = data.info, FC = FC, scaling = scaling)
  }
  colnames(x_T0) <- colnames(x)
  data_only <- x_T0[,colnames(x_T0) %in% row.names(result)]
  dati <- data.frame( x[,data.info], data_only)
  meaned_data <- aggregate(as.matrix(data_only) ~ dati$time + dati$class, data=dati, FUN=median)
  colnames(meaned_data)[1:2] <- c("time", "class")
  mean_data_only <- meaned_data[,3:ncol(meaned_data)]
  p1 <- list()
  for (i in 1:length(unique(dati$class))) {
    cor_matrix <- data_only[which(as.numeric(as.factor(dati$class)) == i),]
    cor_matrix1 <- cor(cor_matrix)
    p1[[i]] <- corrplot(cor_matrix1, method="circle", tl.cex = 0.8, title = unique(dati$class[order(dati$class)])[i])
    p1[[i]][is.na(p1[[i]])] <- 0
    }
  names(p1) <- unique(x$class[order(x$class)])
}
    
  
  