#' @title BCT analysis
#'
#' @param x 
#' @param result 
#' @param data.info 
#' @param title 
#' @param T0 
#' @param time0 
#' @import stringr
#' @import reshape2
#' @import ggplot2
#' @return 
#' @export group_trend
#'
#' @examples
group_trend <- function(x, result = NULL, data.info = c("sample", "subject", "time", "class"),
                        title = "", T0 = F, time0 = NULL) {
  
  factors <- x[,data.info]
  dati <- x
  dati$class <- as.factor(as.character(factors$class))
  dati$time <- as.numeric(factors$time)
    if (is.null(result) != T) {
    nomi <- intersect(row.names(result), colnames(dati))
    data_only <- dati[,nomi]
  } else {
    data_only <- dati[,5:ncol(dati)]
  }
  data_only <- apply(data_only, 2, as.numeric)
   data_fin <- cbind(dati[,1:4], data_only)
 colnames(data_fin) <- str_replace(colnames(data_fin),"[.]x[.]x", "")
 df <- data_fin
 df.m <- melt(df, id.vars = c("sample", "subject", "class", "time"))
  p <- ggplot(data = subset(df.m, !is.na(value)), aes(x= as.numeric(time), y=value, group = paste0(time, class))) +
  theme_bw() +
  stat_summary(aes(color= factor(class), group = factor(class)), fun.data=mean_se, geom="ribbon", alpha=0.25) +
  stat_summary(aes(color= factor(class), group = factor(class)), fun=mean, geom="line", lwd = 2) +
  stat_summary(aes(color= factor(class), group = paste0(factor(variable), factor(class)), linetype = factor(class)), fun="mean", geom="line") +
  scale_linetype_manual(values=c(2,3,4)) +
  geom_vline(xintercept = 2, color = "black", linetype = 5, size = 1) +
  labs(y = "SD", x = "time points", color = "class", linetype = "class") +
  scale_x_continuous(breaks=seq(1,10,1))
  p
  }