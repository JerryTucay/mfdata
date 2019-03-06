
# Create faulty data
balance <- function(data, pi., columns, rows){
  any_error <-  matrix(rep(0, rows*columns), nrow=rows, ncol=columns)
  # size
  sigma <- diag(3)
  s <- matrix(MASS::mvrnorm(rows, 1:columns, sigma), nrow=rows, byrow=TRUE)
  size <- ifelse(s<0, floor(s), ceiling(s))
  # location
  for(i in 1:length(rows)){
    any_error[i, ] <- matrix(rbinom(3,1,pi.[i,]), nrow=1)
  }

  error_frame <- any_error*size
  X <- data
  for(i in columns){
  X[,2:4] <- data[,2:4] + error_frame}
  return(X)
}

#need to specify the amount of rows and colmns
#