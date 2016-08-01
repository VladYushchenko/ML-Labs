legendre <- function (x, n=0){
  ifelse (n == 0, return (rep(1, length(x))),
          ifelse (n == 1, return (x), 
                  return( (2*n-1)/n*x*legendre(x,n-1) - (n-1)/n*legendre(x,n-2) )))
}

walsh <- function(n){
  ifelse (n == 0, return (matrix(1)),
          return(rbind(cbind(walsh(n-1),walsh(n-1)), cbind(walsh(n-1),-walsh(n-1)) )))
}

walsh.normier <- function(x){
  # 0.1 <- -1, 0.9 <- 1
  y <- 0.4*x + 0.5
  return (y)
}
#a)-b)
x <- seq(-1, 1, length.out = 200)
par(mfrow = c(2,3))
for (i in 0:17)
  plot(x, legendre(x,i), type = "l", main = c(paste("Legenre, p =", toString(i))))

# c) -d)
par(mfrow = c(3,3))
for (i in 0:8)
  plot.array(walsh.normier(walsh(i)), main = c(paste("Walsh, n =", toString(i))))