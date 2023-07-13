#' Title
#'
#' @param x 
#' @param result 
#' @param data.info 
#' @param title 
#' @param T0 
#' @param time0 
#'
#' @return
#' @export
#'
#' @examples
trend_plot <- function(x, result = NULL, data.info = c("sample", "subject", "time", "class"),
                       title = "", T0 = F, time0 = NULL) {

library(M3C)
library(reshape2)
library(ggplot2)
library(stringr)
#xstat0 <- zero.norm(xstat, time0 = 0)
  xstat <- x
xstatlog <- cbind(xstat[,1:4], log(xstat[,5:ncol(xstat)] + 0.01))
xstatlog0 <- zero.norm(xstatlog, time0 = 2)
zu <- xstat$class
xstatlog0[which(xstatlog0$time == 0), "class"] <- "A"
azu <- xstat0$class
reslog <- PLSDA_analysis(xstatlog0, comp = 6, vip_value = 2)

CD <- which(xstat1$class == 1)
ND <- which(xstat1$class != 1)
data_only <- xstat1[, nn1]
dataCD <- data_only[CD,]
dataND <- data_only[ND,]
CDab <- M3C(dataCD, cores = 15)
NDab <- M3C(dataND, cores = 15)

dataCD1 <- cbind(xstat1[CD,1:4], dataCD)
dataND1 <- cbind(xstat1[ND,1:4], dataND)

df.mCD1 <- melt(dataCD1, id.vars = c("sample", "subject", "class", "time"))
tdataCD <- t(dataCD)
ass <-CDab$assignments
tdataCD <- as.data.frame(cbind(ass, tdataCD))
df.m <- melt(tdataCD)
}