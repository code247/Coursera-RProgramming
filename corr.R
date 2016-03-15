corr <- function(directory, threshold = 0) {
  files <- list.files(directory, full.names=TRUE)
  n <- length(files)
  t <- 0
  ans <- NULL
  for (i in 1:n) {
    count <- 0
    df <- read.csv(files[i])
    c <- complete.cases(df)
    for (j in seq_along(c)) {
      if(c[j]==TRUE) {
        count <- count + 1
      }
    }
    if (count > threshold) {
      df_subset <- na.omit(df)
      ans[i] <- cor(df_subset["sulfate"],df_subset["nitrate"])
    }
  }
  ans <- na.omit(ans)
  ans
}

