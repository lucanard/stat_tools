#' @Title univar_analysis
#' @name univar_analysis
#' @param NEMix 
#' @param p_value 
#' @description performs t.test of a list of variables comparing the different groups
#' @return to_heat
#' @export univar_analysis
#' @examples
#' univar_analysis(NEMix)
univar_analysis <- function(x, p_value = 0.05) {
anovas <- lapply(NEMix[,5:ncol(x)],
                 function(x) pairwise.t.test(x, x[,"class"],
                                             paired = F,
                                             p.adjust.method = "BH",
                                             pool.sd = T))
anovi <- lapply(seq(1, length(anovas)), function (x) unlist(anovas[[x]][3]))
res <- as.data.frame(do.call(rbind, anovi))
signif <- apply(res, 1, function(x) x <= p_value)
signifies <- which(signif == TRUE)
if (length((levels(as.factor(x[,"class"]))) >= 3) == T) {
signifies <- c(which(signif[1,] == TRUE), which(signif[2,] == TRUE), which(signif[4,] == TRUE))
} else {signifies <- which(signif == TRUE)}
adjusted_signifies <- signifies + 4
NEMi_res <- x[,adjusted_signifies]
to_heat <- cbind(x[,1:4], NEMi_res)
return(to_heat)
}
