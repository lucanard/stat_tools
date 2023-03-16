#' @Title PLSDA_analysis
#' @name PLSDA_analysis
#' @param x
#' @param data.info
#' @param multilevel
#' @param T0
#' @param time0
#' @param FC
#' @param MCCV
#' @param comp
#' @param whole.table
#' @param scaling
#' @param scaled
#' @param best.comp
#' @param model
#' @param vip_value
#' @import mixOmics
#' @description the function performs PCA and PLS-DA analysis on your data and it evaluates the strength of the PLS-DA model using MCCV. 
#' @return VIPs
#' @return plsda1
#' @export PLSDA_analysis
#'
#' @examples
#' PLSDA_analysis(x)
PLSDA_analysis <- function(x, data.info = c("sample", "subject", "time", "class"), multilevel = F,
                           T0 = F, time0 = NULL, FC = F, MCCV = 20, comp = 3, whole.table = F,
                           scaling = T, scaled = F, best.comp = F, model = F, vip_value = 1) {
  if (T0 == T) {
    x <- zero.norm(x, time0 = time0, data.info = data.info, FC = FC, scaling = scaling)
  }
  #if (is.null(time0) == F) {
  #ab <- which(x$time <= time0)
  #x <- x[-ab,]
  #}
  data_only <- x[,(length(data.info)+1):ncol(x)]
X <- as.matrix(data_only)
#X <- as.matrix(X_T0)
factors <- x[,data.info]
design = data.frame(sample = as.numeric(as.factor(factors[,"subject"])))
classes <- factors[,"class"]
if (multilevel == T) {X <- withinVariation(X = X, design = design)}
col.stimu <- rainbow(length(levels(as.factor(classes))))
pca1 <- pca(X, ncomp = comp, scale = scaled)
plotIndiv(pca1, ind.names = paste(as.numeric(factors$time), as.numeric(as.factor(factors$subject)), sep = "_"),
          col = col.stimu, ellipse = T, legend = T, star = F, main='PCA, comp 1 - 2', group = classes)
plsda1 <- splsda(X, Y=classes, ncomp=comp, scale = scaled)
plotIndiv(plsda1, ind.names = paste(as.numeric(factors$time), as.numeric(as.factor(factors$subject)), sep = "_"),
          col = col.stimu, style = "ggplot2", ellipse = T, legend = T, star = T, main='PLS-DA, comp 1 - 2')
perf1 <- perf(plsda1, nrepeat = MCCV, auc = T)
if (best.comp == T) {
best.comp <- round(mean(apply(perf1$error.rate$overall, 2, which.min)))
} else {
  best.comp = comp
}
if (comp != best.comp) {
  print("repeating plsda analysis with the correct number of components")
  plsda1 <- splsda(X, Y=classes, ncomp=best.comp, scale = scale)
  if (best.comp > 1) {
  plotIndiv(plsda1, ind.names = paste(as.numeric(factors$time), as.numeric(as.factor(factors$subject)), sep = "_"),
            col = col.stimu, ellipse = T, legend = T, star = T, main='PLS-DA, comp 1 - 2')
  } else {
    print("no plot allowed: number of compontents lower than 2")
  }
    perf1 <- perf(plsda1, nrepeat = MCCV, auc = T)
print(perf1$auc)
} else {
  print(perf1$auc)
}
plot(perf1)
Vitty <- vip(plsda1)
Vitty <- round(Vitty, 3)
if (whole.table == T) {
Vitties <- cbind(ab, )
ab <- matrix("", 4,3)
colnames(ab) <- c("comp1", "comp2", "comp3")
Vitties <- rbind(ab, Vitty)
new_result <- t(cbind(t(x), Vitties))
write.csv(new_result, file = "DataMatrixVIPs.csv")
return(as.matrix(new_result))
} else {
if (best.comp == 1) {
  VIPs <- Vitty[which(Vitty[,1] >= vip_value),]
  } else {
    VIPs <- list()
    for (i in 1:best.comp) {
VIPs[[i]] <- Vitty[which(Vitty[,i] >= vip_value),]
   }
   VIPs <- unique(do.call(rbind, VIPs))
  }
row.names(VIPs) <- gsub(".x", "", row.names(VIPs))
}
write.csv(VIPs, file = "VIPs.csv")
if (model == T) {
  return(plsda1)
} else {
  return(VIPs)
}
}
