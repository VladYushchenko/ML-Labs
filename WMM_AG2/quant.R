
quant <- function (x, bits = 8) {
  out <- ifelse (x < 0, quant(0),
                 ifelse (x > 1, quant(1),
                         (floor(x*(2**bits)) + 0.5)/(2**bits)) )
  return (out)
}

SNR <- function(x, qx){
  out <- mean(x*x)/mean((x-qx)*(x-qx))
  out <- 10*log10(out)
  return(out)
}

testdata <- cos(seq(0,pi/2, length = 1000))
SNRtestdata8 <- SNR(testdata, quant (testdata, 8))
SNRtestdata12 <- SNR(testdata, quant (testdata, 12))

vecunif <- runif (1000) 
uniform8 <- SNR(vecunif, quant (vecunif, 8))
uniform12 <- SNR(vecunif, quant (vecunif, 12))

vecgauss <- rnorm(1000, 0.5, 0.125)
SNRgauss8 <- SNR(vecgauss, quant (vecgauss, 8))
SNRgauss12 <- SNR(vecgauss, quant (vecgauss, 12))

vecequi <- seq (from=0, to=1, length=1000)
SNRequi8 <- SNR(vecequi, quant (vecequi, 8))
SNRequi12 <- SNR(vecequi, quant (vecequi, 12))

vecunif1 <- runif (1000, max=1.10) 
SNRuni8 <- SNR(vecunif1, quant (vecunif1, 8))
SNRunif12 <- SNR(vecunif1, quant (vecunif1, 12))


C <- seq(from=1, to=8, length=100)

experiment <- array(0, c(100,1000))
experiment[,1] <- C


for(r in 1:100)
  experiment[r,] <- rnorm(1000, mean=0.5, sd=1/(2*experiment[r,1]))

S <- mapply (SNR(experiment[1:100], quant (experiment[1:100])))
