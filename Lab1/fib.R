fib <- function(n) {
  if (n<=0) {
    return(-1)
  }
  else if (n==1) {
    return(0)
  }
  else if (n==2) {
    return(1)
  }
  else {
    a <- 0
    b <- 1
    for (i in 3:n) {
      c <- a + b
      a <- b
      b <- c
    }
    return(b)
  }
}
