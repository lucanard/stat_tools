#' Title
#'
#' @param x 
#' @param result 
#' @param data.info 
#' @param title 
#' @import reshape2
#' @import ggplot2
#' @import stringr
#' @import ggpubr
#' @return 
#' @export create_boxplot
#'
#' @examples
#' create_boxplot(x)
create_boxplot <- function(x, result = NULL, trendplot = T, data.info = c("sample", "subject", "time", "class"), title = "VIPs trends") {

factors <- x[,data.info]
dati <- x
dati$class <- as.factor(as.character(factors$class))
dati$time <- as.numeric(factors$time)
#row.names(result) <- str_replace_all(colnames(x),"[Xx]", "")
if (is.null(result) != T) {
nomi <- intersect(row.names(result), colnames(dati))
data_only <- dati[,nomi]
} else {
  data_only <- dati[,5:ncol(dati)]
}
data_only <- apply(data_only, 2, as.numeric)
#data_only <- scale(data_only, center = T, scale = T)
data_fin <- cbind(dati[,1:4], data_only)
#data_fin <- zero.norm(data_fin, time0 = 0, scaling = T)
#colnames(data_fin) <- str_replace_all(colnames(data_fin),"[Xx]", "")
colnames(data_fin) <- str_replace(colnames(data_fin),"[.]x[.]x", "")
#colnames(data_fin) <- str_replace(colnames(data_fin),"[.]", "_")
#colnames(data_fin) <- str_replace(colnames(data_fin),"_", "")

df <- data_fin
# melting by "Label". `melt is from the reshape2 package.
# do ?melt to see what other things it can do (you will surely need it)
df.m <- melt(df, id.vars = c("sample", "subject", "class", "time"))

#single plot
#ggplot(data = df.m, aes(x=time, y=value)) + geom_boxplot(aes(fill=class)) +
  #ggtitle("progesterone")

#df.m <- hj
#faceting
if (trendplot == T) {
p <- ggplot(data = subset(df.m, !is.na(value)), aes(x= as.numeric(time), y=value, group = interaction(as.numeric(time), as.numeric(as.factor(class)))))
#p <- p + geom_boxplot(aes(color = class, group = paste0(class, time, variable)), width=0.7)
#p <- p + geom_path(aes(color=class, group = paste0(subject)))
#p <- p + stat_summary(aes(color= factor(paste0("t",class)), group = factor(class)), fun = mean, geom = "line", lwd = 1.5, na.rm = TRUE)
p <- p + stat_summary (aes(color= factor(class), group = factor(class)), fun.data=mean_se, geom="ribbon", alpha=0.25)
#p <- p + geom_jitter(aes(color = class, group = paste0(class, variable)), position = "identity", alpha = 0.5)
p <- p + labs(title = "markers", y = "")
p <- p + theme(title = element_text(size = 6, face="bold"), axis.text=element_text(size=6), axis.title=element_text(size=6,face="bold"), text=element_text(size=6))
#p <- p + scale_fill_manual(breaks = c("EGH", "EPO"), values=c("darkgoldenrod1", "deepskyblue2"))
#p <- p + scale_color_manual(breaks = c("EGH", "EPO"), values=c("darkgoldenrod3", "blue2", "darkgoldenrod3", "blue2"))
p <- p + theme_bw()
} else {
  p <- ggplot(data = subset(df.m, !is.na(value)), aes(x= class, y=value, group = class))
  p <- p + geom_boxplot(aes(color = class, group = paste0(class, variable)), width=0.7)
  p + geom_jitter(aes(color = class, group = paste0(class, variable)), position = "identity", alpha = 0.5)
  p <- p + theme_bw()
  p <- p + stat_compare_means(method = "wilcox.test", label.y = -0.05, cex = 3)
  }
#p <- p + geom_path(aes(color=as.factor(subject), group = paste0(subject, variable), linetype = as.factor(class)))
#p <- p + stat_summary(aes(color= class, group = paste0(class, variable)), fun.y=mean, geom="line", lwd = 1)
#p <- p + stat_summary (aes(color= class, group = paste0(class, variable)), fun.data=mean_se, geom="ribbon", alpha=0.25)
#p <- p + geom_jitter(aes(color=class))
p <- p + facet_wrap( ~  variable, scales="free", ncol = 4)
p <- p + xlab("days") + ylab("concentration") + ggtitle("steroids")
p <- p + guides(color=guide_legend(title="classes"), fill = F)
p <- p + theme(axis.text = element_text(size = 6),
              axis.text.x = element_text(size = 6),
               strip.text = element_text(size = 6),
              strip.background = element_rect(size=0.3))
p
}
