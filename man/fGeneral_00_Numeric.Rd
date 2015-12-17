\name{fGeneral_00_Numeric}
\alias{fGeneral_00_Numeric}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fGeneral_00_Numeric(newColumnName, timeInput, functionInput)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{newColumnName}{
%%     ~~Describe \code{newColumnName} here~~
}
  \item{timeInput}{
%%     ~~Describe \code{timeInput} here~~
}
  \item{functionInput}{
%%     ~~Describe \code{functionInput} here~~
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
function (newColumnName, timeInput, functionInput) 
{
    y <- fGeneral_00(timeInput, slope = functionInput[[1]], intersection = functionInput[[2]])
    y <- as.data.frame(y)
    y <- cbind(timeInput, y)
    colnames(y) <- c("hpi", newColumnName)
    return(y)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
