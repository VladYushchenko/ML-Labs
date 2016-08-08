setwd("~/R/WMM_AG9")

# a)
library(MASS)

GDC <- function(x, nf=ncol(x), npc=NULL){
  if (is.null(npc)){
    classified <- qda(x[-nf], x[[nf]])
    object <- list(
      "qda"=classified)
  }
  else{
    stopifnot(npc <= (length(x)-1))
    data <- x[-nf]
    covX <- cov(data)
    eigenPairs <- eigen(covX)
    U <- eigenPairs$vectors[, 1:npc]
  
    x.new <- (data.matrix(data) %*% U)
    classified <- qda(x.new, x[[nf]])
    
    object <- list(
      "qda"=classified,
      "transf.mat"=U)
  }
  class(object) <- "GDC"
  return(object)
}

# b)
predict.GDC <- function(o, newdata){
  
  if (!is.null(o$transf.mat)) {
    newdata <- data.matrix(newdata) %*% o$transf.mat
  }
  result <- predict(o$qda, newdata)
  return(result)
}

# c)
load("heart.rda")
heldout <- function(train, test=train, method=GDC, nf=ncol(train), ...){
  o <- method(train, ...)
  classifications <- predict(o, test[-nf])
  errs <- sum(as.vector(classifications$class) != as.vector(test[[nf]])) / length(classifications$class)
  return (errs)
}

data <- list(heart.lern, heart.test, rbind(heart.lern, heart.test))

N.types <- length(data)

err.mat.all <- matrix(0,N.types,N.types)
for (i in 1:N.types)
  for(j in 1:N.types)
    err.mat.all[i,j] <- heldout(data[[i]], test=data[[j]])
colnames(err.mat.all) <-  c("TEST=heart.learn","TEST=heart.test","TEST=heart.all")
rownames(err.mat.all) <-  c("TRAIN=heart.learn","TRAIN=heart.test","TRAIN=heart.all")
err.all <- as.table(err.mat.all)
print(err.all)


# e)
err.mat.sub <- matrix(0,N.types,N.types)
for (i in 1:N.types)
  for(j in 1:N.types)
    err.mat.sub[i,j] <- heldout(data[[i]], test=data[[j]], npc=6)
colnames(err.mat.sub) <- c("TEST=heart.learn","TEST=heart.test","TEST=heart.all")
rownames(err.mat.sub) <- c("TRAIN=heart.learn","TRAIN=heart.test","TRAIN=heart.all")
err.sub <- as.table(err.mat.sub)
print(err.sub)


# f)
N <- length(heart.lern)-1
m <- 4
x.axes <- seq(1,N)
errors.table  <- matrix(0,m,N)

for (i in 1:13){
  nf <- ncol(heart.lern)
  one <- heldout(cbind(heart.lern[i], heart.lern[nf]), 
                 test=cbind(heart.test[i], heart.test[nf])
  )
  
  two <- heldout(heart.lern[-i], test=heart.test[-i])
  three <- heldout(cbind(heart.lern[1:i], heart.lern[nf]), 
                   test=cbind(heart.test[1:i], heart.test[nf])
  )
  
  four <- heldout(heart.lern, test=heart.test, npc = i)
  errors.table [,i] <- 100*c(one, two, three, four)
}

plot(x.axes, errors.table[1,], 
     xlim = c(1,N), xlab = "Merkmalindex", ylab = "Fehlerrate %", 
     type = "l", col="cyan")
lines(x.axes, errors.table[2,], type = "l", col="blue")
lines(x.axes, errors.table[3,], type = "l", col="green")
lines(x.axes, errors.table[4,], type = "l", col="red")

legend("topright", c("Single", "Knockout" , "Leading", "PC"), 
       lty = 1, col = c("cyan", "blue","green", "red"))