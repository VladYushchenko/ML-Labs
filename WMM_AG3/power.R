DFT.matrix <- function(size){
  tempvec <- seq (from = 0, length.out = size)
  # Koeffizienten in exp-Ausdruck
  powmat <- tempvec %o% tempvec
  # W-Matrix gemäß Definition 
  W <- (1/size)*exp (-2*pi/size*complex(imaginary=powmat))
  W <- matrix(data = W, nrow = size)
  return (W)
}

W <- DFT.matrix(6)
#print(W)
# Dieses Ergebnis hängt von Koefficient bei W-Definition ab
# In unserem Fall conj(W) * W = N*I
# Außerdem wenn 1/sqrt(N) verwendet wird, dann ist W unitär (conj(W) * W = I)
print(Conj(W) %*% W)
print(6*Conj(W) %*% W )
  
powerspec <- function (x){
  result <- DFT.matrix(length(x)) %*% x
  in.interval <- function(y) (Arg(y) > 0 & Arg(y) < pi)
  result <-  Filter (in.interval,  result)
  result <- Mod(result)**2
  result <- 10*log10(result)
  return (result)
}

plot.powerpec <- function (x, t_max, SF) {
  plot(seq(from = 0, to = t_max, length.out = t_max*freq), x, type="l", col="black", ylab="Amplitude", xlab = "msec");
  quadspektr <- powerspec(x)
  plot(seq(from = 0, to = t_max*length(x)/2, length.out = length(quadspektr)), quadspektr, type="l", col="black", ylab="dB", xlab = "HZ");
}

T = 250
freq = 16
fcs <- cos((1:T)*0.4)
fsumsincos <- sin((1:T)*2.5)+cos((1:T)*1)+cos((1:T)*0.3)
funi <- runif(T,-1,+1)
# 25 statt 20, um length == 250
fseq <- rep(seq(0,1,length.out=25),T/25)
fableit <- sin((2:(T+1))*2.5)+cos((2:(T+1))*1)+cos((2:(T+1))*0.3) - fsumsincos
finteg <- cumsum(fsumsincos)

# Korrektheit von DFT
#print(fft(fcs))
#print(DFT.matrix(length(fcs)) %*% fcs * length(fcs))

par(mfrow = c(3,2))
plot.powerpec(fcs, T/16, freq)
plot.powerpec(fsumsincos,T/16, freq)
plot.powerpec(funi,T/16, freq)
plot.powerpec(fseq,T/16, freq)
plot.powerpec(fableit,T/16, freq)
plot.powerpec(finteg,T/16, freq)