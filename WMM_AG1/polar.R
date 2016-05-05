fcos <- function (a,x){
  z <- a*cos(x);
  return(z);
}

fsin <- function (b,x){
  z <- b*sin(x);
  return (z);
}

fsum <- function (a, b, x){
  return (a*cos(x) + b*sin(x));
}

fpol <- function (a, phi, x){
  return (a*cos(x + phi));
}

fcoffA <- function(a,b){
   ak <- sqrt(a*a + b*b);
   return (ak);
}

fcoffP <- function(a,b){
  pk <- atan2(-b,a);
  return (pk);
}

ak <- -6;
bk <- 8;
x <- seq(0,10, by=0.1);
plot (x, fcos(ak, x), ylim=c(-10,10), col="blue", main="Functions", type="l");
lines (x, fsin(bk, x), col="brown", type="l");
lines (x, fsum(ak, bk, x), col="red", type="l");
lines (x, fpol(fcoffA(ak,bk), fcoffP(ak,bk), x), col="green", type="p");
cat("A must be: ",fcoffA(ak,bk), "and phi must be: ", fcoffP(ak,bk))

