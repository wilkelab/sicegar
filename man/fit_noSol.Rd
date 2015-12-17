\name{fit_noSol}
\alias{fit_noSol}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fit_noSol(startList)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{startList}{
%%     ~~Describe \code{startList} here~~
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
function (startList) 
{
    colon_Names <- c("Estimate", "Std_Error", "t_value", "Pr_t")
    row_Names <- names(startList)
    ResultVector1 = character(length = length(startList) * 4)
    ResultVector2 = numeric(length = length(startList) * 4)
    vec1 <- 1:length(startList)
    vec2 <- 1:4
    counter04 = 0
    for (i in vec1) {
        for (j in vec2) {
            counter04 <- counter04 + 1
            ResultVector1[counter04] <- paste(row_Names[i], colon_Names[j], 
                sep = "_")
            ResultVector2[counter04] <- NA
        }
    }
    m02 <- as.data.frame(t(ResultVector2))
    row.names(m02) <- NULL
    colnames(m02) <- ResultVector1
    m02 <- dplyr::mutate(m02, residual_Sum_of_Squares = NA)
    return(m02)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
