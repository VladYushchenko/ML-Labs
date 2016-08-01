#setwd("/home/vlad/R/WMM_AG5/")
load("moment.rda")

translate <- function(x, dr=0, dc=0) {
  y <- x
  n <- nrow(y)
  m <- ncol(y)
  rshift <- dr %% n
  cshift <- dc %% m
  if (rshift){
    y[1:rshift,] <- x[-(1:(n-rshift)),]
    y[-(1:rshift),] <- x[1:(n-rshift),]
  }
  x <- y
  if (cshift){
    y[,1:cshift] <- x[,-(1:(m-cshift))]
    y[,-(1:cshift)] <- x[,1:(m-cshift)]
  }
  return (y)
}

norm.moment <- function (f, p=0, q=0){
  mom.res <- sum(f*(row(f)**p)*(col(f)**q))
  return (mom.res)
}


moment <- function(x, plot=TRUE) {
  x.cord <- norm.moment(x,1,0)/norm.moment(x,0,0)
  y.cord <- norm.moment(x,0,1)/norm.moment(x,0,0)
  
  y.shift <- translate(x,dr=-trunc(x.cord), dc=trunc(y.cord))
  
  mu11 <- norm.moment(y.shift,1,1) 
  mu20 <- norm.moment(y.shift,2,0) 
  mu02 <- norm.moment(y.shift,0,2) 
  
  angle <- 0.5*atan(2*mu11 / (mu20 - mu02))
  angle <- angle*180/pi
  
  if (plot){
    plot.array(x, main = "Zeppelin")
    abline(h = x.cord, v = y.cord, untf = FALSE, col = "red")
    mtext(c(paste("Neigungswinkel ", toString(round(angle,1)))), side=1)
  }
  
  return (c(x.cord, y.cord, angle))
}


moments.all <- function(x) {
  moment(x)
  moment(1-x)
  moment(binarise(x))
  moment(binarise(1 - x))
}

par(mfrow = c(2,2))
moments.all(zeppelin1)
moments.all(zeppelin2)
moments.all(zeppelin3)
moments.all(zeppelin4)
moments.all(zeppelin5)
moments.all(zeppelin6)
moments.all(zeppelin7)
moments.all(zeppelin8)


