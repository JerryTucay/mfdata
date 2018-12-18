

nmar <- function(data, p, q, column = NULL){
  if(is.null(column)){
    MDS <- data.frame(apply(data, 2, function(x){
      cutoff <- max(x)*p
      x.na <- x
      idx  <- which(x>cutoff)
      x.na[idx] <- ifelse(rbinom(n=length(idx), size=1, prob=q)==1, NA, x.na[idx])
      return(x.na)
    }))
  }else{
    MDS <- data
    for(i in 1:length(column)) {
      cutoff <- max(MDS[ ,column[i]])*p
      x.na <- MDS[ ,column[i]]
      idx <- which(x.na > cutoff)
      MDS[idx ,column[i]] <- ifelse(rbinom(n = length(idx), size = 1, prob = q)==1, NA, x.na[idx])
    }
  }
  return(MDS)
}