\name{nmar}
\alias{nmar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
NMAR: Not missing at random
}
\description{
nmar() alows the user to forceibly input missing values (NA) that replicates being not missing at random. This kind of missing depends strictly on the variable's value. If Xi > p\% of max value for Xi then there is q\% chance
the data point is missing.}
\usage{
nmar(data, p, q, column)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
The data frame that you want to put missing values into.
}
  \item{p}{
Percent of the the max value in the specified column
}
  \item{q}{
  Percent chance the Xi data point is missing given Xi> p\% of max
}
\item{column}{
The columns from the data varaiable you want to input missing values into. For multiple colums use c() with the specifed column choices.
}
}
\details{
Definintly play around with p and q if you want to get a specfic amount of data missing.
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
function (data, p = 0.7, q = 0.5)
{
    MDS <- data.frame(apply(data, 2, function(x) {
        cutoff <- max(x) * p
        x.na <- x
        idx <- which(x > cutoff)
        x.na[idx] <- ifelse(rbinom(n = length(idx), size = 1,
            prob = q) == 1, NA, x.na[idx])
        return(x.na)
    }))
    return(MDS)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
