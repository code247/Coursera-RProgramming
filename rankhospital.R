rankhospital <- function(state, outcome, num = "best") {
  df <- read.csv("outcome-of-care-measures.csv")
  a <- unique(df[,7])
  c <- 0
  s <- data.frame();
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
  if(outcome =="heart failure") t <- 17
  if( outcome == "pneumonia") t <- 23
  s <- df[df$State==state,]
  s[,t] <- sapply(s[,t],as.character)
  s[,t] <- sapply(s[,t],as.numeric)
  # x <- as.vector(sub[,t])
  # m <- min(sub[,t], na.rm = TRUE)
  s <- s[order(s[,t],s[,2], decreasing = FALSE),]

  if (num == "best")
    ans <- as.vector(s$Hospital.Name[1])
  else if (num == "worst") {
    s <- s[order(s[,t],s[,2], decreasing = TRUE),]
    ans <- as.vector(s$Hospital.Name[1])
  }
  else if (num > nrow(s))
    ans <- NA
  else
    ans <- as.vector(s$Hospital.Name[num])
  ans
}