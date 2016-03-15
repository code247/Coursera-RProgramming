pollutantmean <- function (directory, pollutant, id=1:332) {
  files <- list.files(directory, full.names=TRUE)
  n <- length(id)
  df<- data.frame()
  for (i in id[1]:id[n]) {
    df <- rbind(df,read.csv(files[i]))            
  }
  df_subset <- df[, pollutant]
  m <- mean(df_subset,na.rm =TRUE)
  m
}