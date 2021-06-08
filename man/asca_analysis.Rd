\name{asca_analysis}
\alias{asca_analysis}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{performing ANOVA-simultaneous components analysis on your data
%%  ~~function to do ... ~~
}
\description{the scripts determine which parameters are statistically significant to discriminate your data.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
asca_analysis(x, data.info = c("sample", "subject", "time", "class"), permutations = 100, plots = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{data.info}{
%%     ~~Describe \code{data.info} here~~
}
  \item{permutations}{
%%     ~~Describe \code{permutations} here~~
}
  \item{plots}{
%%     ~~Describe \code{plots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, data.info = c("sample", "subject", "time", "class"), 
    permutations = 100, plots = F) 
{
    data_only <- x[, (length(data.info) + 1):ncol(x)]
    X <- data_only
    ab <- which(colSums(X) == 0)
    if (length(ab) != 0) {
        X <- X[, -ab]
    }
    factors <- x[, data.info[2:4]]
    factors[, 3] <- as.numeric(as.factor(factors[, 3]))
    factors[, 1] <- as.numeric(as.factor(factors[, 1]))
    matrix <- NULL
    for (i in 1:ncol(factors)) {
        matrix <- c(matrix, combn(ncol(factors), i, simplify = FALSE))
    }
    matrix <- lapply(matrix, paste0)
    matrix2 <- as.numeric(sapply(seq(1, length(matrix)), function(x) paste0(as.numeric(matrix[[x]]), 
        collapse = "")))
    matrix2 <- paste(matrix2, collapse = ",")
    if (any(factors[, 2] <= 0)) {
        factors[, 2] <- factors[, 2] + abs(min(factors[, 2]))
    }
    factors <- apply(factors, 2, function(x) as.numeric(x))
    ASCA_data1 <- apply(X, 2, function(x) as.numeric(x))
    my_ASCA_res <- ASCA.Calculate(data = ASCA_data1, levels = as.matrix(factors), 
        equation.elements = matrix2, scaling = T)
    ASCA.GetSummary(my_ASCA_res, quietly = FALSE)
    permutation <- ASCA.DoPermutationTest(my_ASCA_res, perm = permutations)
    matrix <- NULL
    for (i in 1:ncol(factors)) {
        matrix <- c(matrix, combn(colnames(factors), i, simplify = FALSE))
    }
    matrix <- lapply(matrix, paste0)
    matrix2 <- sapply(seq(1, length(matrix)), function(x) paste(matrix[[x]], 
        collapse = " x "))
    colnames(permutation) <- matrix2
    return(permutation)
    if (plots == T) {
        ASCA.Plot(my_ASCA_res)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
