### validation ###
#' @Title validation
#' @description the function valdates a PLS-DA model using various forms of validations, including MCCV and permutation tests.
#' @param x a PLSDA model obtained from the function PLSDA_analysis
#' @param data.info the columns containing descriptive information regarding your table. default = data.info = c("sample", "subject", "time", "class")  
#' @param T0 boolean: if TRUE, it normalizes the data according to the beginning of the treatment. default = FALSE
#' @param MCV boolean: if TRUE, it performs Monte Carlo cross validation. default = FALSE
#' @param controls indicates the lines where the control samples are located
#' @param time0 the time of the beginning of the treatment
#' @param comp number of componnents to be used in the PLS-DA model. default = 3
#' @param ntest number of tests to be executed during the validation. default = 20
#' @param LOSOXV performs leave one subject out cross validation. default = FALSE
#' @param permutime performs permutation of samples across time
#' @param permuclass performs permutation of samples across class
#' @import mixOmics
#' @import dplyr
#' @import purrr
#' @return result
#' @export validate
#' @examples
#' validate(x)
validate <- function(x, data.info = c("sample", "subject", "time", "class"), T0 = F, MCV = F, controls,
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
