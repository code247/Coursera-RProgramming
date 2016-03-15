rankall <- function (outcome, num) {
  df <- read.csv("outcome-of-care-measures.csv",na.strings=c("Not Available"))
  a <- unique(df[,7])
  a <- a[order(a, decreasing = FALSE)]
  t <- 0
  ans <- data.frame(hospital=NA, state=NA)
  if (outcome != "heart attack" && outcome != "heart failure" && outcome != "pneumonia") 
    stop('invalid outcome')
  if(outcome == "heart attack") 
    t <- 11
  if(outcome =="heart failure") 
    t <- 17
  if(outcome =="pneumonia")
    t <- 23
  for (i in 1:length(a)) {
    s <- df[df$State==a[i],]
    s <- s[complete.cases(as.numeric(s[,t])),]
    x <- as.numeric(s[,t])
    y <- as.character(s[,2])
    s <- s[order(x,y, decreasing = FALSE), ]
    if (num == "best")
      num <- 1
    if (num == "worst") {
      s <- s[order(x,y, decreasing = T), ]
      if (i == 1) {
        ans <- data.frame(hospital=s$Hospital.Name[1],state=a[1])
      } else {
        ans <- rbind(ans,c(as.vector(s$Hospital.Name[1]),as.vector(a[i])))      
      }
    }
    if (i == 1 & num != "worst") {
      ans <- data.frame(hospital=s$Hospital.Name[num],state=a[1])
    } else {
      ans <- rbind(ans,c(as.vector(s$Hospital.Name[num]),as.vector(a[i])))      
    }
  }
  ans
}