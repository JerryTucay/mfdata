# we used p=.5 and q=.2
# should i just set p=.5 and not let them choose
# column = works with an input of c() and of course just a single value

mar <- function(data, p, q, column){
  MDS <- data
  U  <- runif(NROW(data))
    m.idx <- (rbinom(n=NROW(data), size=1, prob=q*(U > p))==1)
    MDS[m.idx,column] <- NA
  return(MDS)
}
