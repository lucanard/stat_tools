### validation ###
#' Title
#'
#' @param x 
#' @param data.info 
#' @param T0 
#' @param MCV 
#' @param controls 
#' @param time0 
#' @param comp 
#' @param ntest 
#' @param LOSOXV 
#' @param permutime 
#' @param permuclass 
#' @import mixOmics
#' @import dplyr
#' @import purrr
#'
#' @return
#' @export validate
#'
#' @examples
#' validate(x)
validate <- function(x, data.info = c("sample", "subject", "time", "class"), T0, MCV = F, controls,
                        time0, comp = 3, ntest = 20, LOSOXV = F, permutime = F, permuclass = F) {
  
  finres <- list(length = length(ntest))
  classi <- list(length = length(ntest))
    datas <- x
    if (LOSOXV == T) {
    LOSOXVres <- list(length = max(unique(datas$subject)))
    for (i2 in unique(datas$subject)) {
      testset <- datas[datas$subject == i2, ]
      trainset <- datas[datas$subject != i2, ]
      if (permutime == T) {
        tv <- aggregate(testset[,"time"], list(testset[,"subject"]), sample)
        tv <- tv[,-1]
        time <- as.vector(t(tv))
        testset$time <- time
      }
      if (T0 == T) {
        trainset <- zero.norm(trainset, time0 = time0)
        testset <- zero.norm(testset, time0 = time0)
      } else {
        trainset <- trainset
        testset <- testset
      }
      X <- as.matrix(trainset[,5:ncol(trainset)])
      classes <- trainset[,"class"]
      plsda1 <- splsda(X, Y=classes, ncomp=comp, scale = F)
      test_data <- testset[,5:ncol(testset)]
      predictions <- predict(object = plsda1, newdata = test_data, dist = "max.dist")
      predictions <- as.data.frame(predictions$class$max.dist[,max(ncol(predictions$class$max.dist))])
      colnames(predictions) <- "prediction"
      #LOSOVXres[[i2]] <- ifelse(as.character(predictions[,1]) == as.character(testset$class), 0, 1)
      LOSOXVres[[i2]] <- cbind(predictions, testset$class)
    }
    result <- LOSOVXres
    } else {
      for (i in 1:ntest) {
        if (permutime == T) {
          if (MCV == T) {
            test <- sample(nrow(datas), 6)
            test <- test[!(test %in% controls)]
            testset <- datas[test,]
            trainset <- datas[-test, ]
          } else {
            trainset <- datas
            testset <- datas
          }
          tv <- aggregate(testset[,"time"], list(testset[,"subject"]), sample)
        tv <- tv[,-1]
        time <- as.vector(t(tv))
        testset$time <- time
        } else {
          if (MCV == T) {
            test <- sample(nrow(datas), 6)
            test <- test[!(test %in% controls)]
            testset <- datas[test,]
            trainset <- datas[-test, ]
          } else {
            trainset <- datas
            testset <- datas
          }
        }
          if (T0 == T) {
            trainset <- zero.norm(trainset, time0 = time0)
            testset <- zero.norm(testset, time0 = time0)
          }
    X <- as.matrix(trainset[,5:ncol(trainset)])
  classes <- trainset[,"class"]
  plsda1 <- splsda(X, Y=classes, ncomp=comp, scale = F)
  X <- as.matrix(testset[,5:ncol(testset)])
if (permuclass == T) {
  data_only <- data_only[sample(nrow(data_only), nrow(data_only)),]
}
predictions <- predict(object = plsda1, newdata = X, dist = "max.dist")
predictions <- as.data.frame(predictions$class$max.dist)
finres[[i]] <- ifelse(as.character(predictions[,3]) == as.character(testset$class), 0, 1)
    }
result <- finres
  }
  return(result)
}
