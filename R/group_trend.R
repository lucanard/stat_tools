#' @title group_trend
#' @param x 
#' @param result 
#' @param data.info 
#' @param title 
#' @param T0 
#' @param time0 
#' @import stringr
#' @import reshape2
#' @import ggplot2
#' @return p
#' @export group_trend
#' @examples
#' group_trend(x)
group_trend <- function(x, result = NULL, data.info = c("sample", "subject", "time", "class"),
                        title = "", T0 = F, time0 = NULL, FC = F, scaling = T) {
  
  if (T0 == T) {
    x <- zero.norm(x, time0 = time0, data.info = data.info, FC = FC, scaling = scaling)
  }
  colnames(x) <- str_replace(colnames(x),"[.]x[.]x", "")
  colnames(x) <- str_replace(colnames(x),"[.]x", "")
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
  #stat_summary(aes(color= factor(class), group = paste0(factor(variable), factor(class)), linetype = factor(class)), fun="mean", geom="line") +
  scale_linetype_manual(values=c(2,3,4)) +
  geom_vline(xintercept = 2, color = "black", linetype = 1, size = 0.5) +
    #geom_vline(xintercept = 6, color = "black", linetype = 5, size = 0.5) +
    #geom_vline(xintercept = 12, color = "black", linetype = 5, size = 0.5) +
    #geom_vline(xintercept = 18, color = "black", linetype = 5, size = 0.5) +
    #geom_vline(xintercept = 24, color = "black", linetype = 5, size = 0.5) +
    #geom_vline(xintercept = 36, color = "black", linetype = 5, size = 0.5) +
  labs(y = "SD", x = "time points", color = "class", linetype = "class") +
  #scale_x_continuous(breaks=seq(min(as.numeric(factors$time)),max(as.numeric(factors$time)), as.numeric(factors$time)))
    scale_x_continuous(breaks=seq(1,10,1))
    p
  return(p)
  }