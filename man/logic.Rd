\name{logic}
\alias{logic}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Logic Error
}
\description{
Intended to enter values of two variable that shouldnt occure at the same time.
This kind of error occurs when structural zeros occure. A structural zero happens in cases where it is impossible for the certain choice to be made. For example someone who has says no to having sex then says yes to having sex in the past month.
}
\usage{
logic(data, phi, var_choice)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
The data that you want to give missing values to
}
  \item{phi}{
  the probability that a row will have a logic error. Can enter a single number or an array of numbers to give weight to certian observations.
  }
   \item{var_choice}{
  Enter the two variables and their value that can never happen at the same time.
  
  EX: error<-("gamm = 100, norm = 100")
  As of now its critical to include the spaces verbatum to the example. Currently in the works to fix this.
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

#phi as a single number
df <- data.frame(gamm=round(rgamma(1000, 3),0), norm=round(rnorm(1000,5), 0), pois=round(rpois(1000,2), 2))
error <- ("gamm = 100, norm = 100")

df_with_error <- LOGIC(df, .5, error )



}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
