\name{fit_main}
\alias{fit_main}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fit_main(data, fitFunction, startList, lowerBounds, upperBounds, min_Factor, n_iterations, n_runs_max, n_runs_min)
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
  \item{n_runs_max}{
%%     ~~Describe \code{n_runs_max} here~~
}
  \item{n_runs_min}{
%%     ~~Describe \code{n_runs_min} here~~
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
    min_Factor, n_iterations, n_runs_max, n_runs_min) 
{
    count01 <- 1
    print(count01)
    count01 <- count01 + 1
    counter02a <- 0
    counter02b <- 0
    flag <- 0
    storage <- NULL
    while (counter02a < n_runs_max & counter02b < n_runs_min) {
        counter02a <- counter02a + 1
        m01 <- try(fit_S1(data, fitFunction, startList, lowerBounds, 
            upperBounds, min_Factor, n_iterations), silent = FALSE)
        nameList <- names(startList)
        startList <- runif(length(startList), 0, 1) * (upperBounds - 
            lowerBounds) + lowerBounds
        names(startList) <- nameList
        startList <- as.list(startList)
        if (class(m01) != "try-error") {
            flag <- 1
            counter02b <- counter02b + 1
            if (is.null(storage) == 1) {
                storage <- m01
            }
            else {
                if (m01$residual_Sum_of_Squares < storage$residual_Sum_of_Squares) {
                  storage <- m01
                }
            }
        }
    }
    m01 <- storage
    if (flag == 0) {
        m01 <- fit_noSol(startList)
    }
    m01$n_runs_actual <- counter02a
    m01$n_runs_meaningful <- counter02b
    m01$minGFP <- min(data$GFP)
    m01$maxGFP <- max(data$GFP)
    print(m01)
    if (!is.numeric(m01$residual_Sum_of_Squares)) {
        browser()
    }
    if (!is.numeric(m01$log_likelihood)) {
        browser()
    }
    if (!is.numeric(m01$AIC_value)) {
        browser()
    }
    if (!is.numeric(m01$BIC_value)) {
        browser()
    }
    return(m01)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
