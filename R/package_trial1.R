# column = works with an input of c() and of course just a single value

mcar <- function(data, p, column){
  MDS <- data
  N <- NROW(data)
    m.idx <- sample(x=1:NROW(data), size= p*NROW(data), replace=FALSE)
    MDS[m.idx ,column] <- NA
  return(MDS)}
