\name{fit_S1}
\alias{fit_S1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fit_S1(data, fitFunction, startList, lowerBounds, upperBounds, min_Factor, n_iterations)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{fitFunction}{
%%     ~~Describe \code{fitFunction} here~~
}
  \item{startList}{
%%     ~~Describe \code{startList} here~~
}
  \item{lowerBounds}{
%%     ~~Describe \code{lowerBounds} here~~
}
  \item{upperBounds}{
%%     ~~Describe \code{upperBounds} here~~
}
  \item{min_Factor}{
%%     ~~Describe \code{min_Factor} here~~
}
  \item{n_iterations}{
%%     ~~Describe \code{n_iterations} here~~
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
function (data, fitFunction, startList, lowerBounds, upperBounds, 
    min_Factor, n_iterations) 
{
    print(rapply(startList, c))
    z <- nlsLM(fitFunction, data[, c("inputData", "hpi")], start = startList, 
        control = list(maxiter = 500, minFactor = 1/2^20), lower = lowerBounds, 
        upper = upperBounds, trace = F)
    m02 <- summary(z)$parameters
    ResultVector1 = character(length = prod(dim(m02)))
    ResultVector2 = numeric(length = prod(dim(m02)))
    vec1 <- 1:dim(m02)[1]
    vec2 <- 1:dim(m02)[2]
    counter03 = 0
    for (i in vec1) {
        for (j in vec2) {
            counter03 <- counter03 + 1
            ResultVector1[counter03] <- paste(rownames(m02)[i], 
                colnames(m02)[j], sep = "_")
            ResultVector2[counter03] <- m02[i, j]
        }
    }
    m03 <- as.data.frame(t(ResultVector2))
    row.names(m03) <- NULL
    colnames(m03) <- ResultVector1
    colnames(m03) <- gsub("__", "_", gsub("[. ]", "_", names(m03)))
    colnames(m03) <- gsub("_$", "_Pr_t", gsub("Pr.*", "", names(m03)))
    rSSq = sum((as.vector(resid(z)))^2)[1]
    lLhood = as.vector(logLik(z))[1]
    AICval = as.vector(AIC(z))[1]
    BICval = as.vector(BIC(z))[1]
    m03 <- m03 \%>\% dplyr::mutate(residual_Sum_of_Squares = rSSq, 
        log_likelihood = lLhood, AIC_value = AICval, BIC_value = BICval)
    if (!is.numeric(rSSq)) {
        browser()
    }
    if (!is.numeric(lLhood)) {
        browser()
    }
    if (!is.numeric(AICval)) {
        browser()
    }
    if (!is.numeric(BICval)) {
        browser()
    }
    return(m03)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
