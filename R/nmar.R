nmar <- function(data, p, q, column = NULL){
  if(is.null(column)){
    MDS <- data.frame(apply(data, 2, function(x){
      cutoff <- max(x)*p
      x.na <- x
      idx  <- which(x > cutoff)
      x.na[idx] <- ifelse(rbinom(n=length(idx), size=1, prob=q)==1, NA, x.na[idx])
      return(x.na)
    }))
    
    df_wide <- rbind( data, MDS) %>% mutate( ds = c(rep("df", NROW(data)), rep("df_miss", NROW(MDS))  ))
    
    long<- melt(df_wide, id.vars = "ds")
    
    densplot <-ggplot(long, aes(x=value, fill = ds)) + 
      geom_density(alpha=.4) + 
      facet_wrap(~variable, ncol=2, scales = "free")
    
    misspercplot <- gg_miss_var(MDS, show_pct = T)
  }else{
    MDS <- data
    for(i in 1:length(column)) {
      cutoff <- max(MDS[ ,column[i]])*p
      x.na <- MDS[ ,column[i]]
      idx <- which(x.na > cutoff)
      MDS[idx ,column[i]] <- ifelse(rbinom(n = length(idx), size = 1, prob = q)==1, NA, x.na[idx])
    }
    data_par<-data[, column]
    MDS_par<-MDS[, column]
    
    df_wide <- rbind( data_par, MDS_par) %>% mutate( ds = c(rep("df", NROW(data)), rep("df_miss", NROW(MDS))  ))
    
    long<- melt(df_wide, id.vars = "ds")
    
    densplot <-ggplot(long, aes(x=value, fill = ds)) + 
      geom_density(alpha=.4) + 
      facet_wrap(~variable, ncol=2, scales = "free")
    
    misspercplot <- gg_miss_var(MDS, show_pct = T)
  }
  output<- list(MDS, densplot, misspercplot)
  return(output)
}
