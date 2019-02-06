
# Create faulty data
balance <- function(data, pi.){
  any_error <-  matrix(rep(0, 1000*3), nrow=1000, ncol=3)
  # size
  sigma <- diag(3)
  s <- matrix(MASS::mvrnorm(1000,c(0,0,0), sigma), nrow=1000, byrow=TRUE)
  size <- ifelse(s<0, floor(s), ceiling(s))
  # location
  for(i in 1:1000){
    any_error[i, ] <- matrix(rbinom(3,1,pi.[i,]), nrow=1)
  }

  error_frame <- any_error*size
  X <- data
  X[,2:4] <- data[,2:4] + error_frame
  return(X)
}
