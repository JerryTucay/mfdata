\name{mar}
\alias{mar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
MAR: Missing at Random
}
\description{
mar() alows the user to forceibly input missing values (NA) that replicates being missing at random. This kind of missing data depends on the value of another variable in the dataset. The function uses the uniform distribution to act as the variable that the missing data is dependent on. If Ui > p then there is q\% chance your value will is missing in the i'th spot.
}
\usage{
mar(data, p, q, column)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
the dataframe that you want to input NAs into
}
  \item{p}{
Value to compare the uniform to. A choice of .5 will make the expression Ui>p true about half of the time.
  }
  \item{q}{
percent chance that the i'th value will be missing given Ui > p.
  }
  \item{column}{
The column(s) in the data that you want to give missing values to. For multiple colums use c() with the specifed column choices.
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
## Inputs missing data in the first two columns of the data set df

df<- data.frame(x=rnorm(100, 10, 2), y=rpois(100,4), z=rbinom(100, 1, .4))
df_missing<- mar(data = df, p = .5, q = .2, column = 1:2)
sum(is.na(df_missing))/200

## Inputs missing data into all of the columns in df2

df2<- data.frame(x=rnorm(100, 10, 2), y=rpois(100,4), z=rbinom(100, 1, .4))
df_missing2<- mcar(data = df, p = .5, q = .2)
sum(is.na(df_missing2))/300
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
