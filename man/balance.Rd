\name{mcar}
\alias{mcar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
MCAR: Missing completely at random
}
\description{
mcar() alows the user to forceibly input missing values (NA) that replicates being missing completly at random. This kind of missing data is missing by random chance.
}
\usage{
mcar(data, p, column = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
The data that you want to give missing values to
}
  \item{p}{
  The percentage of data that you want to make missing.
  }
   \item{column = NULL}{
  When NULL the function will run through your whole data set
  Set a value to target specific columns
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

## Inputs missing data in the first two colums of the data set df

df<- data.frame(x=rnorm(100, 10, 2), y=rpois(100,4), z=rbinom(100, 1, .4))
df_missing<- mcar_test(df, .25, 1:2)
sum(is.na(df_missing))/200

## Inputs missing data into all of the columns in df2

df2<- data.frame(x=rnorm(100, 10, 2), y=rpois(100,4), z=rbinom(100, 1, .4))
df_missing2<- mcar_test(df2, .25)
sum(is.na(df_missing2))/300

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
