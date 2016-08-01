setwd("~/R/WMM_AG6")
load('lpc.rda')

### (a) ###

layout(matrix(1:2, 2, 2))

plot(sound)
spectrum(sound)

### (b) ###

modspec <- function(x, n, m){
  model <- ar(x)
  gain <- sqrt(model$var.pred)
  
  cat("gain: ", gain, "\n")
  
  vals <- model$ar[1:n]
  vals <- c(1, -vals) # Add a0 at the end
  
  # Fill the rest up with zeros
  vals_new <- rep(0, 2 * (m - 1))
  vals_new[1:length(vals)] <- vals
  
  vals <- vals_new
  x.dft <- fft(vals)
  x.log_dft <- 2*log10(gain) - 2*log10(Mod(x.dft))
  x_plot <- seq(0,1000,by=2)
  
  plot(x_plot, x.log_dft[1:length(x_plot)], type="l")
  #plot(seq(0, length(x_plot), length=length(x.log_dft)), x.log_dft, type="l")
}

spectrum(sound, method="ar")
modspec(x=sound, n=28, m=512)

### (c) ###

resonance <- function(x, n, ...){
  model <- ar(x)
  vals <- c(-1,model$ar[1:n])
  
  roots <- 1/polyroot(vals)# / exp(1i * Arg(vals) * seq_along(vals))
  cv <- seq(0, 2 * pi, length=1000)
  plot(sin(cv), cos(cv), type="l", asp=1)
  
  points(roots[Im(roots) > 0], pch=16, col="green", bg="green", cex=3)
  points(roots[Im(roots) < 0], pch=16, col="blue", bg="blue", cex=3)
  #points(Re(roots[Im(roots) > 0]), Im(roots[Im(roots) > 0]), pch=16, col="green", bg="green", cex=3)
  #points(Re(roots[Im(roots) <= 0]), Im(roots[Im(roots) <= 0]), pch=16, col="blue", bg="blue", cex=3)
  
  pts <- Arg(roots[Im(roots) > 0]) - pi/2
  cat(pts, "\n")
  
  points(sin(pts), cos(pts), pch=16, col="red", bg="red")
  pts
}

### (d) ###

layout(matrix(1:9, 3, 3, byrow=T))

for(i in 1:9 * 2){
  resonance(sound, i, main=paste("model order ", i))
}

### (e) ###

layout(matrix(1:2, 2, 1))

freqs <- resonance(sound, 10)
spectrum(sound, method="ar")

abline(v= 1000/(2*pi)*(freqs + pi/2), col="red")