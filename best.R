best <- function(state, outcome) {
  df <- read.csv("outcome-of-care-measures.csv")
  a <- unique(df[,7])
  n <- names(df)
  c <- 0
  t <- 0
  for (i in 1:length(a)) {
    if(state == a[i]) 
      break
    else 
      c <- c + 1
  }
  if(c==54) stop('invalid state')
  if (outcome != "heart attack" && outcome != "heart failure" && outcome != "pneumonia") 
    stop('invalid outcome')
  if(outcome == "heart attack") t <- 11
  else if(outcome =="heart failure") t <- 17
  else if( outcome == "pneumonia") t <- 23
  sub <- subset(df, df$State==state)
  sub[,t] <- sapply(sub[,t],as.character)
  sub[,t] <- sapply(sub[,t],as.numeric)
  # x <- as.vector(sub[,t])
  m <- min(sub[,t], na.rm = TRUE)
  s <- subset(sub, sub[,t]==m)
  s <- s[order(s$Hospital.Name),]
  ans <- as.vector(s$Hospital.Name[1])
  ans
}