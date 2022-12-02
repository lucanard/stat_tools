#' @Title time0_subtraction
#' @description 
#' @param x
#' @param time0
#' @param data.info
#' @param meaning
#' @param percentage
#' @param FC
#' @param scaling
#' @import stringr
#' @import dplyr
#' @import purrr
#'
#' @return NEM_norm
#' @export zero.norm
#'
#' @examples
#' zero.norm(x, time0 = 0)
zero.norm <- function(x, time0 = NULL, data.info = c("sample", "subject", "time", "class"), 
                      meaning = F, percentage = F, FC = F, scaling = T) {
  if (is.null(time0) == T) {stop("please add your time 0")}
   foldchange <- function(x, y) {
    res <- (x-y)/y
    return(res)
  }
  perce <- function(x,y) {
    res <- (x/y)*100
    return(res)
  }
  if (percentage == T) {
    cols = c((length(data.info)+1): ncol(x))
    x[,cols] <- lapply(x[,cols], function(x) replace(x, x == 0, min(x[x>0], na.rm = TRUE)))
  }
  T0 = time0
  if (meaning == T) {
  untreated <- x[x[,data.info[3]] <= T0,]
  } else {
    untreated <- x[x[,data.info[3]] == T0,]
  }
treated <- x[x[,data.info[3]] > T0,]
tutti_con <- aggregate(untreated[, (length(data.info) +1):ncol(untreated)], list(untreated[,"subject"]), median)
colnames(tutti_con)[1] <- "subject"
df14 <- left_join(x, tutti_con, by = "subject")
if (FC == T) {
  NEM_norm <- cbind(id=df14[,1], map2_df(df14[,(length(data.info) +1):ncol(x)], df14[,(ncol(x) +1):ncol(df14)], `foldchange`))
} 
if (percentage == T) {
  NEM_norm <- cbind(id=df14[,1], map2_df(df14[,(length(data.info) +1):ncol(x)], df14[,(ncol(x) +1):ncol(df14)], `perce`))
} else {
NEM_norm <- cbind(id=df14[,1], map2_df(df14[,(length(data.info) +1):ncol(x)], df14[,(ncol(x) +1):ncol(df14)], `-`))
}
NEM_norm <- cbind(x[,1:length(data.info)], NEM_norm)
NEM_norm <- NEM_norm[,-which(colnames(NEM_norm) =="id")]
if (scaling == T) {
tutti_scal <- aggregate(NEM_norm[, (length(data.info) +1):ncol(NEM_norm)], list(NEM_norm[,"subject"]), sd)
colnames(tutti_scal)[1] <- "subject"
df14 <- left_join(NEM_norm, tutti_scal, by = "subject")
NEM_norm <- cbind(id=df14[,1], map2_df(df14[,(length(data.info) +1):ncol(x)], df14[,(ncol(x) +1):ncol(df14)], `/`))
NEM_norm <- cbind(x[,1:length(data.info)], NEM_norm)
NEM_norm <- NEM_norm[,-which(colnames(NEM_norm) =="id")]
}
return(NEM_norm)
}
