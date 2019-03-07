
# Create faulty data
balance <- function(data, pi, choice){
  rows<-nrow(data)
  columns<- ncol(data)
  any_error <-  matrix(rep(0, nrow(data)*3), nrow=nrow(data), ncol=ncol(data))
  # size
  sigma <- diag(3)
  s <- matrix(MASS::mvrnorm(rows, 1:columns, sigma), nrow=rows, byrow=TRUE)
  size <- ifelse(s<0, floor(s), ceiling(s))
  # location
  for(i in 1:rows){
    any_error[i, ] <- matrix(rbinom(3,1,pi[i,]), nrow=1)
  }

  error_frame <- any_error*size
  X <- data
  for(i in length(choice)){
  X[,choice[i]] <- data[,choice[i]] + error_frame}
  
  output <- list(X, any_error)
  return(output)
}

#need to specify the amount of rows and colmns
#