load ("filter2D.rda")

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
mat.normier <- function(x){
  # y <- y/a + b
  # a = max() - min()
  # b = -min()/(max() - min())
  return ((x - min(x))/(max(x) - min(x)))
}

filter.mean.3x3 <- function(x) {
  n <- nrow(x)
  m <- ncol(x)
  y <- matrix(rep(0,n*m), nrow = n)
  coef <- expand.grid(-1:1,-1:1)
  for (i in -1:1)
    for (j in -1:1)
      y <- y + translate(x,i,j)
  return (y/9)
}

filter.robert <- function(x, res.norm = TRUE){
  temp1 <- abs(x - translate(x,-1,-1))
  temp2 <- abs(translate(x,-1,0) - translate(x,0,-1))
  result <- temp1+temp2
  if (res.norm)
    result <- mat.normier(result)
  return (result)
}
filter.sobel.h <- function(x, res.norm = TRUE){
  y <- translate(x,-1,-1) + 2*translate(x,-1,0) + translate(x,-1,1)
  y <- y - translate(x,1,-1) - 2*translate(x,1,0) - translate(x,1,1)
  y <- y/8
  if (res.norm)
    y <- mat.normier(y)
  return (y)
}
filter.sobel.v <- function(x, res.norm = TRUE){
  y <- translate(x,-1,1) + 2*translate(x,0,1) + translate(x,1,1) 
  y <- y - translate(x,-1,-1) - 2*translate(x,0,-1) - translate(x,1,-1)
  y <- y/8
  if (res.norm)
    y <- mat.normier(y)
  return (y)
}

filter.grad.mag <- function(x, res.norm = TRUE){
  y <- sqrt(filter.sobel.v(x, res.norm = FALSE)**2 + filter.sobel.h(x, res.norm = FALSE)**2)
  if (res.norm)
    y <- mat.normier(y)
  return (y)
}

filter.grad.angle <- function(x, res.norm = TRUE){
y <- atan2(filter.sobel.h(x, res.norm = FALSE),filter.sobel.v(x, res.norm = FALSE))
  if (res.norm)
    y <- mat.normier(y)
  return (y)
}
mulaw <- function(x, mu=100){
  res <- mat.normier(x)
  res <- log(1+mu*x)/log(1+mu)
  return (res)
}

#a)
par(mfrow = c(2,2))
plot.array(translate(Lessing,24,64))
plot.array(translate(Lessing,-24,64))
plot.array(translate(Lessing,24,-64))
plot.array(translate(Lessing,-24,-64))

#b)-d)
par(mfrow = c(3,2))
plot.array(filter.mean.3x3(Lessing))
plot.array(filter.robert(Lessing))
plot.array(filter.sobel.v(Lessing))
plot.array(filter.sobel.h(Lessing))
plot.array(filter.grad.mag(Lessing))
plot.array(filter.grad.angle(Lessing))

#e)
lessing.kant <- 180/pi*filter.grad.angle(Lessing, res.norm = FALSE)
Grad.betrag <- filter.grad.mag(Lessing, res.norm = FALSE)
med.betrag <- median(Grad.betrag)
lessing.kant.filtered <- lessing.kant[Grad.betrag > med.betrag]

par(mfrow = c(2,1))
hist (lessing.kant, col="red", breaks=32, xlab="Grad")
hist (lessing.kant.filtered, col="black", breaks=32, xlab="Grad")

#f)
par(mfrow = c(2,3))
plot.array(filter.grad.mag(algae))
plot.array(filter.grad.mag(couple))
plot.array(filter.grad.mag(muscle))
plot.array(filter.grad.mag(cashmere))
plot.array(filter.grad.mag(ludwig))
plot.array(filter.grad.mag(tonga))

#g)
par(mfrow = c(2,3))
plot.array(mulaw(filter.grad.mag(algae)))
plot.array(mulaw(filter.grad.mag(couple)))
plot.array(mulaw(filter.grad.mag(muscle)))
plot.array(mulaw(filter.grad.mag(cashmere)))
plot.array(mulaw(filter.grad.mag(ludwig)))
plot.array(mulaw(filter.grad.mag(tonga)))
