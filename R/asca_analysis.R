#' @Title performs asca analysis
#'
#' @param x
#' @param data.info
#' @param permutations
#' @param plots
#' @import stringr
#' @import MeTStaT
#' @return permutation
#' @export asca_analysis
#'
#' @examples
asca_analysis <- function(x, data.info = c("sample", "subject", "time", "class"),
                          permutations = 100, plots = F) {
  
    data_only <- x[,(length(data.info)+1):ncol(x)]
  X <- data_only
  ab <- which(colSums(X) == 0)
  if (length(ab) != 0) {X <- X[,-ab]}
  factors <- x[,data.info[2:4]]
  factors[,3] <- as.numeric(as.factor(factors[,3]))
  factors[,1] <- as.numeric(as.factor(factors[,1]))
  matrix <- NULL
  for(i in 1:ncol(factors)) {matrix <- c(matrix,combn(ncol(factors),i,simplify=FALSE))}
  matrix <- lapply(matrix, paste0)
  matrix2 <- as.numeric(sapply(seq(1, length(matrix)), function(x) paste0(as.numeric(matrix[[x]]), collapse = "")))
  matrix2 <- paste(matrix2, collapse = ",")
  #factors[,2] <- as.numeric(as.factor(factors[,2]))
    if (any(factors[,2] <= 0)) {
    factors[,2] <- factors[,2] + abs(min(factors[,2]))
    }
  factors <- apply(factors, 2, function(x) as.numeric(x))
    ASCA_data1 <- apply(X, 2, function(x) as.numeric(x))
    my_ASCA_res <- ASCA.Calculate(data = ASCA_data1, levels = as.matrix(factors),
                                equation.elements = matrix2, scaling = T)
  ASCA.GetSummary(my_ASCA_res, quietly = FALSE)
  permutation <- ASCA.DoPermutationTest(my_ASCA_res, perm = permutations)
      matrix <- NULL
  for(i in 1:ncol(factors)) {matrix <- c(matrix,combn(colnames(factors),i,simplify=FALSE))}
  matrix <- lapply(matrix, paste0)
  matrix2 <- sapply(seq(1, length(matrix)), function(x) paste(matrix[[x]], collapse = " x "))
  colnames(permutation) <- matrix2
  return(permutation)
  if (plots == T) {
    ASCA.Plot(my_ASCA_res)
  }
  }
