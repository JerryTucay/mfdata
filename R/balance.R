
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
  
  df_wide <- rbind( data, X) %>% mutate( ds = c(rep("df", NROW(data)), rep("df_error", NROW(X))  ))
  
  long<- melt(df_wide, id.vars = "ds")
  
  densplot <-ggplot(long, aes(x=value, fill = ds)) + 
    geom_density(alpha=.4) + 
    facet_wrap(~variable, ncol=2, scales = "free_x")
  
  output <- list(X, any_error, densplot)
  return(output)
}

#need to specify the amount of rows and colmns
#