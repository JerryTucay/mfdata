# column = works with an input of c() and of course just a single value

mcar <- function(data, p, column = NULL){
  MDS <- data
  if (is.null(column)) {
    for (i in 1:ncol(MDS)) {
      m.idx <- sample(x=1:NROW(data), size= p*NROW(data), replace=FALSE)
      MDS[m.idx , i] <- NA
    }
    df_wide <- rbind( data, MDS) %>% mutate( ds = c(rep("df", NROW(data)), rep("df_miss", NROW(MDS))  ))
    
    long<- melt(df_wide, id.vars = "ds")
    
    densplot <-ggplot(long, aes(x=value, fill = ds)) + 
      geom_density(alpha=.4) + 
      facet_wrap(~variable, ncol=2, scales = "free_x")
    
    misspercplot <- gg_miss_var(MDS, show_pct = T)
  } else {
    for(i in 1:length(column)){
      m.idx <- sample(x=1:NROW(data), size= p*NROW(data), replace=FALSE)
      MDS[m.idx ,column[i]] <- NA}
    
    data_par<-data[, column]
    MDS_par<-MDS[, column]
    
    df_wide <- rbind( data_par, MDS_par) %>% mutate( ds = c(rep("df", NROW(data)), rep("df_miss", NROW(MDS))  ))
    
    long<- melt(df_wide, id.vars = "ds")
    
    densplot <-ggplot(long, aes(x=value, fill = ds)) + 
      geom_density(alpha=.4) + 
      facet_wrap(~variable, ncol=2, scales = "free_x")
    
    misspercplot <- gg_miss_var(MDS, show_pct = T)
    }

  output<- list(MDS, densplot, misspercplot)
  return(output)
}
