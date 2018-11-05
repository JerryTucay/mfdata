

nmar <- function(data, p, q, column){
  refined_data<- data[, column]
  MDS <- data.frame(apply(data, 2, function(x){
    cutoff <- max(x)*p
    x.na <- x
    idx  <- which(x>cutoff)
    x.na[idx] <- ifelse(rbinom(n=length(idx), size=1, prob=q)==1, NA, x.na[idx])
    return(x.na)
  }))
  return(MDS)
}
