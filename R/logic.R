


 # LOGIC <- function(data, phi){
 #FD <- data
 #e2 <- rbinom(NROW(FD), 1, phi)
 #FD$y4[e2 == 1] <- 0
 #FD$y5[e2 == 1] <- 1
 #return(FD)
 #}


LOGIC <- function(data, phi, var_choice){
  FD <- data
  e2 <- rbinom(NROW(FD), 1, phi)
  
  var_comma <- str_split(var_choice, ", ")
  
  for(i in 1:length(var_choice))
  {
    var_equal <- str_split(var_comma[[i]], " = ")
    var1<-which(colnames(df) == var_equal[[1]][1] )
    var2<-which(colnames(df) == var_equal[[2]][1] )
    
    FD[,var1][e2 == 1] <- var_equal[[1]][2]
    FD[,var2][e2 == 1] <- var_equal[[2]][2]
    
    type1<- class(data[ ,var1])
    type2<- class(data[ ,var2])
    
    if(type1 == "integer")
    {
        FD[,var1]<- as.integer(as.character(FD[ ,var1]))
    }else     if(type1 == "numeric")
    {
        FD[,var1]<- as.numeric(as.character(FD[ , var1]))
    }else     if(type2 == "integer")
    {
        FD[,var1]<- as.integer(as.character(FD[ ,var1]))
    }else   if(type2 == "numeric")
    {
        FD[,var1]<- as.numeric(as.character(FD[ , var1]))
    }
    
  }
  return(FD)
}
