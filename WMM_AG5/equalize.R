#setwd("/home/vlad/R/WMM_AG5/")
load("equalize.rda")

equailize <- function(x)  {
  
  if (length(dim(x)) == 2){
  # grauwert-Bild
  x.rounded <- trunc(x*255)
  nr <- nrow(x)
  nc <- ncol(x)
  # histogramm grauwert-counts 
  x.breaks <- seq(0,1,length.out = 257)
  y <- hist(x, breaks = x.breaks , plot = FALSE)
  y.relativ <- y$counts/(nr*nc)
  # kumulative histogramm
  y.cum <- cumsum(y.relativ)
  
  # transformation
  result <- x
  result <- matrix(floor(255*y.cum[x.rounded+1]), nrow = nr, ncol = nc)/255
  return (result)
  }
  else {
    d.res <- x
    d.res[,,1] <- equailize(d.res[,,1])
    d.res[,,2] <- equailize(d.res[,,2])
    d.res[,,3] <- equailize(d.res[,,3])
    return (d.res)
  }
}


plot.equalize <- function (x, main="", K=64) {
  par(mfrow = c(3,2))
  
  x.equalized <- equailize(x)
  plot.array(x, main = main)
  plot.array(x.equalized, main = paste(main, " equalized" ))
  
  x.breaks <- seq(0,1,length.out = K+1)
  h1 <- hist(x, breaks = x.breaks, main = "Absolute", xlab = "Grauwert", ylab = "Bildpunkte")
  h2 <- hist(x.equalized , breaks = x.breaks, main = "Absolute", xlab = "Grauwert", ylab = "Bildpunkte")
  
  x.cum <- cumsum(h1$counts/nrow(x)/ncol(x))
  x.equ.cum <- cumsum(h2$counts/nrow(x)/ncol(x))
  
  plot(0:(K-1), x.cum, type = "s", main = "Kumulative", xlab = "Anzahl Grauwertzellen", ylab = "Wahrscheinlichkeit" )
  plot(0:(K-1), x.equ.cum, type = "s", main = "Kumulative", xlab = "Anzahl Grauwertzellen", ylab = "Wahrscheinlichkeit")
}

plot.equalize(algae, main = "algae")
plot.equalize(couple, main = "couple")
plot.equalize(Lessing, main = "Lessing")
plot.equalize(mri1, main = "mri1")
plot.equalize(soil, main = "soil")
plot.equalize(tank, main = "tank")
plot.equalize(turbinate, main = "turbinate")
plot.equalize(usound, main = "usound")
plot.equalize(xray, main = "xray")
plot.equalize(GUESS, main = "Guess")

par(mfrow = c(2,1))
plot.array(mandrill)
plot.array(equailize(mandrill))
