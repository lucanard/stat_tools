#' Title
#'
#' @param x 
#' @param result 
#' @param data.info 
#' @param title 
#' @param T0 
#' @param time0 
#' @param distmet 
#' @param hclustmet 
#' @import RColorBrewer
#' @import gplots
#' @importFrom heatmap.plus heatmap.plus
#' @import stringr
#' @return
#' @export do_heatmap
#'
#' @examples
#' do_heatmap(x)
do_heatmap <- function (x, result = NULL, data.info = c("sample", "subject", "time", "class"),
                        title = "", T0 = F, time0 = NULL, distmet = "euclidean",
                        hclustmet = "ward.D2") {
  if (is.null(result) == F) {
    data_only <- x[,row.names(result)]
  } else {
    data_only <- x[,(length(data.info)+1):(ncol(x))]
    #data_only[, colSums(is.na(data_only)) != nrow(data_only)] <- NULL
    #data_only[,which(colSums(data_only) == 0)] <- NULL
  }
  dati <- data.frame(x[,data.info], data_only)
  if (T0 == T) {
   dati <- zero.norm(dati, data.info = data.info, time0 = time0)
 }
  #colnames(x) <- str_replace_all(colnames(x),"[Xx]", "")
data_only <- dati[,(length(data.info)+1):(ncol(dati))]
row.names(data_only) <- dati[,1]
factors <- x[,data.info]
CLASSES <- dati$class

group1 <- as.numeric(as.factor(as.character(CLASSES)))
color_scale1 <- c("blue", "yellow", "red", "green")
CLASSES <- color_scale1[match(group1,   as.numeric(as.factor(color_scale1)))]
SUBJECT <- as.numeric(factors$subject)
n <- max(unique(as.numeric(dati$subject)))
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
color_scale <- sample(col_vector, n)
SUBJECT <- color_scale[match(SUBJECT,   as.numeric(as.factor(color_scale)))]
TIME <- as.numeric(as.factor(factors$time))
n <- max(unique(as.numeric(as.factor(dati$time))))
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
color_scale <- sample(col_vector, n)
TIME <- color_scale[match(TIME,   as.numeric(as.factor(color_scale)))]
distCor   <- function(x) dist(x, method = distmet)
hclustAvg <- function(x) hclust(x, method=hclustmet)
scale_data <- scale(data_only, center = T, scale = T)
scale_data[which(scale_data <= -3)] <- -3
scale_data[which(scale_data >= 3)] <- 3
myCols = cbind(CLASSES, TIME, SUBJECT)

p <- heatmap.plus(t(as.matrix(scale_data)), hclustfun = hclustAvg, distfun = distCor, scale= "none",
                  Colv= T, Rowv= T, col = colorpanel(n = nrow(scale_data), low= "blue", mid = "black",
                                                     high = "red"), labCol = colnames(t(data_only)),
                  labRow = row.names(t(data_only)), cexCol = 0.8, cexRow = 0.8, margins = c(4,8), ColSideColors = myCols,
                  main = title)

legend("topright",legend=unique(dati$class),col=unique(CLASSES),lwd=5,lty=1,cex=0.7, inset=c(0,0))
#legend("topright", legend=unique(group), col=unique(color1), lwd=5,lty=1,cex=0.7, inset=c(0,0))
}
