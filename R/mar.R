# we used p=.5 and q=.2
# column = works with an input of c() and of course just a single value

mar <- function(data, p, q, column = NULL){
  MDS <- data
  U  <- runif(NROW(data))
  
  if (is.null(column)) {
    for (i in 1:ncol(MDS)) {
      m.idx <- (rbinom(n=NROW(data), size=1, prob=q*(U > p))==1)
      MDS[m.idx , i] <- NA
    }
  } else {
    for(i in 1:length(column)){
      m.idx <- (rbinom(n=NROW(data), size=1, prob=q*(U > p))==1)
      MDS[m.idx ,column[i]] <- NA}}
  
  return(MDS)
}
