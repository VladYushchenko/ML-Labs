#setwd("~/R/WMM_AG7")
load("lda.rda")

#a)
plot.lfd <- function(x, subset=(1:ncol(x))[-nf], nf=ncol(x), main='', ...) {
  plot(x[,subset], col=rainbow(nlevels(x[[nf]]))[x[[nf]]], main=main, ...)
}
#b,c)
class.scatter <- function(x, nf=ncol(x)){
  #
  # x - input dataset
  # nf - factor position
  #
  # result - list with stats
  #
  data <- x[-nf]
  mean <- apply(data, 2, mean)
  # standart covariance
  total <- cov(data)
  nfact <- nlevels(x[[nf]])
  data.factor <- unclass(x[[nf]])
  
  # compute mean vectors for every class
  class.mean <- matrix(0, nfact, length(data))
  for (i in 1:nfact)
    class.mean[i,] <- apply(data[data.factor == i, ], 2, mean)
  
  # compute within class covariance
  within <- matrix(0, nrow(total), ncol(total))
  for (i in 1:nfact){
    temp <- sweep(data[data.factor == i, ], 2, class.mean[i,], FUN = "-")
    within<- within + t(as.matrix(temp))%*%as.matrix(temp)
  }
  within <- within/(nrow(x)-1)    # n - 1 to unbias the cov
  
  # compute the number of elements in class
  number.elem <- rep(0, nfact)
  for (i in 1:nfact)
    number.elem[i] <- nrow(data[data.factor == i, ])
  
  # compute between class covariance
  between <- matrix(0, nrow(total), ncol(total))
  for (i in 1:nfact){
    temp <- class.mean[i,] - mean
    between <- between + number.elem[i]*temp%o%temp
  }
  between <- between/(nrow(x)-1)   # n - 1 to unbias the cov
  
  eps <- 1e-6   # precision for comparison
  stopifnot (total - (within + between) < eps)
  return (list(Mean=mean, Total=total, Within=within, Between=between))
}

#d,e)
LDA <- function(x, train=x, n=length(x)-1, method=c('LDA','PCA','BSA')) {
  #
  # x -original data
  # train - data for stats computations
  # n - number of axes to consider
  # method - applied trasformations
  # 
  # result - list of transformed dataframes
  #
  data <- x[-length(x)]
  train.stats <- class.scatter(train)
  
  # calculations for each method type
  f <- function(s) {
  covX <- switch(s, 
                  LDA = solve(train.stats$Within) %*% train.stats$Between,
                  BSA = train.stats$Between,
                  train.stats$Total)
  eigenPairs <- eigen(covX)
  eigenMat <- eigenPairs$vectors[,1:n]
  lambda <- eigenPairs$values[1:n]
  
  data <- sweep(data, 2, train.stats$Mean, FUN = "-")
  res <- cbind(data.matrix(data) %*% eigenMat, x[length(x)])
  }
  result <- lapply(method, f)
} 

#f)
show.data <- function(x, main = ""){
  par(mfrow = c(2,2))
  plot.lfd(x, subset = 1:2, main = main)
  x.transf <- LDA(x)
  plot.lfd(x.transf[[3]], subset = 1:2, main = paste(main, "BSA"))
  plot.lfd(x.transf[[1]], subset = 1:2, main = paste(main,"LDA"))
  plot.lfd(x.transf[[2]], subset = 1:2, main = paste(main, "PCA"))
}


datasets <- list(ADIDAS=ADIDAS, Diabetes=diabetes, DNA=dna, FIAT=FIAT, Vehicle=vehicle, Mafia=mafia)

#for(i in seq_along(datasets)){
  #show.data(datasets[[i]], main=names(datasets)[i])
#}

#g)

#mafia.train <- cbind(mafia[, 10:19], mafia[23])

mafia.train <- mafia[mafia[[ncol(mafia)]] == "+criminal", ]
print(mafia.train)
mafia.train[[ncol(mafia.train)]] <- factor(mafia.train[[ncol(mafia.train)]], levels = "+criminal")
x.transf <- LDA(mafia, train=mafia.train, n =5,   method = "LDA")
plot.lfd(x.transf[[1]], main = paste("tr=+cr", "LDA"))




