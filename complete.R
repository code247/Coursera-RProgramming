complete <- function(directory, id = 1:332) {
  files <- list.files(directory, full.names=TRUE)
  df<- data.frame()
  count <- 0
  cum_count <- 0
  i_count <- 0
  n <- length(id)
  i <- id[1]
  op <- data.frame(id=NA,nobs=NA)
  for (i in id) {
    cum_count <- count
    count <- 0
    df <- rbind(df,read.csv(files[i]))
    cc <- complete.cases(df)
    for (j in seq_along(cc)) {
      if(cc[j]==TRUE) {
        count <- count + 1
      }
    }
    i_count <- count - cum_count
    if (i == id[1]) {
      op <- data.frame(id=i,nobs=i_count)
    } else {
      op <- rbind(op,c(i,i_count))      
    }
  }
  op
}