na_interpolate <- function(vector){
  last <- first <- rep(0, length(vector))
  last.ind <- 1
  if(is.na(vector[1])){
    last[1] <- 0
    last.ind <- 0
  }else last[1] <- 1
  for(i in 2:length(vector)){
    if(is.na(vector[i])){
      last[i] <- last.ind
    } else {
      last[i] <- i
      last.ind <- i
    }
  }
  first.ind <- length(vector)
  if(is.na(vector[length(vector)])){
    first[length(vector)] <- 0
    first.ind <- 0
  }else first[length(vector)] <- length(vector)
  for(i in (length(vector)-1):1){
    if(is.na(vector[i])){
      first[i] <- first.ind
    } else {
      first[i] <- i
      first.ind <- i
    }
  }
  for(i in 1:length(vector)){
    if(is.na(vector[i])){
      if(last[i]==0) vector[i] <- vector[first[i]]
      else if(first[i]==0) vector[i] <- vector[last[i]]
      else{
      x1 <- ifelse(last[i]==0, 0, vector[last[i]])
      x2 <- ifelse(first[i]==0, 0, vector[first[i]])
      d1 <- ifelse(last[i]==0, 0, i - last[i])
      d2 <- first[i] - i
      vector[i] <- (x1*d2 + x2*d1)/(d1 + d2)
      }
    }
  }
  round(vector, 2)
}


vector <- c(NA, 1:4, NA, NA, 5:8, NA)
vector
na_interpolate(vector)
na_interpolate (c (4,NA,5,NA,NA,NA,8,10,NA,NA))
