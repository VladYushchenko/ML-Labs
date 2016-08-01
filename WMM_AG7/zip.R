### (a) ###
#load("C:\\Users\\Matthias\\Dropbox\\Programmierung\\R\\Uebungen_Werkzeuge_ME_ML\\7\\zip.rda")
#load("zip.rda")

### (b) ###

bits <- function(x, compress=T){
  if(compress){
    return(sum(nchar(memCompress(x, type="gzip"), type="bytes") * 8))
  }
  
  return(sum(nchar(c(x), type="bytes") * 8))
}

### (c) ###

Xentropy <- function(xp, xq){
  (bits(c(xq, xp)) - bits(xq)) / bits(xp, F)
}

### (d) ###

divergence <- function(xp, xq){
  Xentropy(xp, xq) - Xentropy(xp, xp)
}

distance <- function(X){
  dm <- matrix(0, length(X), length(X))
  coefs <- as.matrix(expand.grid(1:nrow(dm), 1:ncol(dm)))
  dm1 <- mapply( function(i, j) divergence(X[[i]], X[[j]]) + divergence(X[[j]], X[[i]]), 
                coefs[,1], coefs[,2])
  dm <- matrix(dm1, nrow(dm))
  
  rownames(dm) <- names(X)
  colnames(dm) <- names(X)
  
  distm <- as.dist(dm)
  distm
}

### (e) ###

library(cluster)

#testlist <- list(Luxemburgish=text[["Luxemburgish"]], German=text[["German"]], English=text[["English"]], French=text[["French"]], Italian=text[["Italian"]], Spanish=text[["Spanish"]])
#testlist

dm <- distance(text)

#dm

par(ask=T)
plot(agnes(dm))
plot(diana(dm))