# column = works with an input of c() and of course just a single value

mcar <- function(data, p, column = NULL){
  MDS <- data
  if (is.null(column)) {
    for (i in 1:ncol(MDS)) {
      m.idx <- sample(x=1:NROW(data), size= p*NROW(data), replace=FALSE)
      MDS[m.idx , i] <- NA
    }
  } else {
    for(i in 1:length(column)){
      m.idx <- sample(x=1:NROW(data), size= p*NROW(data), replace=FALSE)
      MDS[m.idx ,column[i]] <- NA}}
  bmiss<-ggplot(data, aes(x=norm))+ geom_histogram()
  amiss<-ggplot(MDS, aes(x=norm))+ geom_histogram()
  
  output<- list(MDS,bmiss,amiss)
  return(output)
}
