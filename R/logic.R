
LOGIC <- function(data, phi){
  FD <- data
  e2 <- rbinom(NROW(FD), 1, phi)
  FD$y4[e2 == 1] <- 0
  FD$y5[e2 == 1] <- 1
  return(FD)
}
