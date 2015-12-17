\name{clusterFunction}
\alias{clusterFunction}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
clusterFunction(clusterNo, all_fP, all_long_fP)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{clusterNo}{
%%     ~~Describe \code{clusterNo} here~~
}
  \item{all_fP}{
%%     ~~Describe \code{all_fP} here~~
}
  \item{all_long_fP}{
%%     ~~Describe \code{all_long_fP} here~~
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
function (clusterNo, all_fP, all_long_fP) 
{
    if (clusterNo == 1) {
        trashold_noSignal_x = 0.01
        trashold_noSignal_y = 0.1
        trashold_lysis_a <- 0
        trashold_lysis_b <- 0.75
        trashold_AIC = -10
        temp <- dplyr::summarize(all_fP, cluster = if (is.na(linear_slope_Estimate_Corr) | 
            is.na(dif_AIC_value) | is.na(mod10_A2_Estimate)) {
            "NA"
        }
        else if (linear_slope_Estimate_Corr < trashold_noSignal_x & 
            (max_nRat - min_nRat) < trashold_noSignal_y) {
            "no signal"
        }
        else if (mod05_AIC_value > trashold_AIC & mod10_AIC_value > 
            trashold_AIC) {
            "ambiguous"
        }
        else if (dif_AIC_value > trashold_lysis_a & mod10_A2_Estimate < 
            trashold_lysis_b & mod10_M1_Estimate - 0.5 * 4/mod10_B1_Estimate < 
            0) {
            "ambiguous"
        }
        else if (dif_AIC_value > trashold_lysis_a & mod10_A2_Estimate < 
            trashold_lysis_b) {
            "infection & lysis"
        }
        else if (mod05_M1_Estimate - 0.5 * 1.5 * mod05_Ka_Estimate/mod05_B1_Estimate < 
            0) {
            "ambiguous"
        }
        else if (mod05_M1_Estimate + 0.5 * 1.5 * mod05_Ka_Estimate/mod05_B1_Estimate > 
            24) {
            "ambiguous"
        }
        else {
            "infection"
        })
    }
    return(temp)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
