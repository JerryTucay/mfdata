\name{nmar}
\alias{nmar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
NMAR: Not missing at random
}
\description{
nmar() alows the user to forceibly input missing values (NA) that replicates being not missing at random. This kind of missing depends strictly on the variable's value. If Xi > p\% of max value for Xi then there is q\% chance the data point is missing.}
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
\item{column = NULL}{
 When NULL the function will run through your whole data set
 Set a value to target specific columns
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
## Inputs missing data in all the columns in df

df<- data.frame(x=rnorm(100, 10, 2), y=rpois(100,4), z=rbinom(100, 1, .4))
df_missing<- nmar(df, .25)
sum(is.na(df_missing))/300

## Inputs missing data for columns one and three of df2

df2<- data.frame(x=rnorm(100, 10, 2), y=rpois(100,4), z=rbinom(100, 1, .4))
df_missing2<- nmar(df2, p=.5, q=.2 , c(1,3))
sum(is.na(df_missing2))/200
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
