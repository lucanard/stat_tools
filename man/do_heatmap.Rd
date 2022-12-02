\name{do_heatmap}
\alias{do_heatmap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Creating heatmap of your data
%%  ~~function to do ... ~~
}
\description{the fuction creates an heatmap starting from a standard table with sample, subjects, time and class columns as descriptors
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
do_heatmap(x, result = NULL, data.info = c("sample", "subject", "time", "class"), title = "", T0 = F, time0 = NULL, distmet = "euclidean", hclustmet = "ward.D2")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{result}{
%%     ~~Describe \code{result} here~~
}
  \item{data.info}{
%%     ~~Describe \code{data.info} here~~
}
  \item{title}{
%%     ~~Describe \code{title} here~~
}
  \item{T0}{
%%     ~~Describe \code{T0} here~~
}
  \item{time0}{
%%     ~~Describe \code{time0} here~~
}
  \item{distmet}{
%%     ~~Describe \code{distmet} here~~
}
  \item{hclustmet}{
%%     ~~Describe \code{hclustmet} here~~
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
\author{Luca Narduzzi
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
function (x, result = NULL, data.info = c("sample", "subject", 
    "time", "class"), title = "", T0 = F, time0 = NULL, distmet = "euclidean", 
    hclustmet = "ward.D2") 
{
    if (is.null(result) == F) {
        data_only <- x[, row.names(result)]
    }
    else {
        data_only <- x[, (length(data.info) + 1):(ncol(x))]
    }
    dati <- data.frame(x[, data.info], data_only)
    if (T0 == T) {
        dati <- zero.norm(dati, data.info = data.info, time0 = time0)
    }
    data_only <- dati[, (length(data.info) + 1):(ncol(dati))]
    row.names(data_only) <- dati[, 1]
    factors <- x[, data.info]
    CLASSES <- dati$class
    group1 <- as.numeric(as.factor(as.character(CLASSES)))
    color_scale1 <- c("blue", "yellow", "red", "green")
    CLASSES <- color_scale1[match(group1, as.numeric(as.factor(color_scale1)))]
    SUBJECT <- as.numeric(factors$subject)
    n <- max(unique(as.numeric(dati$subject)))
    qual_col_pals = brewer.pal.info[brewer.pal.info$category == 
        "qual", ]
    col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, 
        rownames(qual_col_pals)))
    color_scale <- sample(col_vector, n)
    SUBJECT <- color_scale[match(SUBJECT, as.numeric(as.factor(color_scale)))]
    TIME <- as.numeric(as.factor(factors$time))
    n <- max(unique(as.numeric(as.factor(dati$time))))
    qual_col_pals = brewer.pal.info[brewer.pal.info$category == 
        "qual", ]
    col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, 
        rownames(qual_col_pals)))
    color_scale <- sample(col_vector, n)
    TIME <- color_scale[match(TIME, as.numeric(as.factor(color_scale)))]
    distCor <- function(x) dist(x, method = distmet)
    hclustAvg <- function(x) hclust(x, method = hclustmet)
    scale_data <- scale(data_only, center = T, scale = T)
    scale_data[which(scale_data <= -3)] <- -3
    scale_data[which(scale_data >= 3)] <- 3
    myCols = cbind(CLASSES, TIME, SUBJECT)
    p <- heatmap.plus(t(as.matrix(scale_data)), hclustfun = hclustAvg, 
        distfun = distCor, scale = "none", Colv = T, Rowv = T, 
        col = colorpanel(n = nrow(scale_data), low = "blue", 
            mid = "black", high = "red"), labCol = colnames(t(data_only)), 
        labRow = row.names(t(data_only)), cexCol = 0.8, cexRow = 0.8, 
        margins = c(4, 8), ColSideColors = myCols, main = title)
    legend("topright", legend = unique(dati$class), col = unique(CLASSES), 
        lwd = 5, lty = 1, cex = 0.7, inset = c(0, 0))
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
