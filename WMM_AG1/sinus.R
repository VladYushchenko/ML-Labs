fsin <- function(a, omega, x, phi){
  return (a*sin(2*pi*omega*x + phi));
}

fadded <- function(x){
  z <- 0;
  for (i in 1:6)
    z <- z + fsin(1/i,i, x, 0);
  return (z);
}

faddedwurzel <- function(x){
  z <- 0;
  for (i in 1:6)
    z <- z + fsin(1/sqrt(i),i, x, 0);
  return (z);
}

faddedpoly <- function(x){
  z <- 0;
  for (i in 1:6)
    z <- z + fsin(1/2**i,i, x, 0);
  return (z);
}

faddedphase <- function(x){
  z <- 0;
  for (i in 1:6)
    z <- z + fsin(1/i,i, x, pi/2);
  return (z);
}

faddeduneven <- function(x){
  z <- 0;
  for (i in 0:5) {
    j <- 2*i+1;
    z <- z + fsin(1/j,j, x, 0);
  }
  return (z);
}

x <- seq(0,1, by=0.01);

par(mfrow = c(2,3));
curve(fsin(1,1, x, 0), col="red", ylab="f1, .. , f6");
for (i in 2:6){
  lines(x, fsin(1/i,i, x, 0), col=i);
}

curve(fadded(x), col="black", ylab="f1+...+f6")
curve(faddedwurzel(x), col="black", ylab="A= 1/sqrt(n)")
curve(faddedpoly(x), col="black", ylab="A= 1/2**n")
curve(faddedphase(x), col="black", ylab="Phase = pi/2")
curve(faddeduneven(x), col="black", ylab="f1+f3+...+f11")

