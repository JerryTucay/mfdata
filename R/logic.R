


LOGIC <- function(data, phi){
  FD <- data
  e2 <- rbinom(NROW(FD), 1, phi)
  FD$y4[e2 == 1] <- 0
  FD$y5[e2 == 1] <- 1
  return(FD)
}


string <- c("var 1 = B, var 2 = D", "var 3 = 2, var 4 = 10","var 3 = 2, var 4 = 10")
string

string2<-str_split(string, ",")
string2

trimws(string2[[1]])

vec<- rep(NA, 2)
mylist<- rep(list(vec), 2*length(string2))



  var<-str_split(string2[[1]], "=")
  trimws(var[[1]])
  




string3 <- rep(NA, 4)
for(i in 1:length(string2))
{
  
    mylist[[]] <- str_split(string2[[2]], " = ")

}

string3

string3[[1]][2]
length(string2)
