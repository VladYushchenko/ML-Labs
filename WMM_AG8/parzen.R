#setwd("~/R/WMM_AG8")
load("parzen.rda")

#a)
parzen <- function(x,sd){
  object <- list(
              support = x, 
              sigma = sd
  )
  class(object) <- append(class(object),"parzen")
  return(object)
}

#b)
predict.parzen <- function(o, newdata){
  result <- sapply(newdata, FUN=function(x){
    (1 / length(o$support)) * sum(dnorm (x - o$support, sd = o$sigma))
  })
  return(result)
}

#c)
plot.parzen <- function(o,
                        xlim=c(min(o$support) - 3*o$sigma,max(o$support) + 3*o$sigma),
                        xlab=paste("Sigma=", round(o$sigma,3)),
                        ...){
  fn <- function(x){
    predict.parzen(o, x)
  }
  curve(fn, xlim=xlim, xlab=xlab, type="l",...)
  rug(o$support, lwd = 1, col = "red")
}


# d)
par(mfrow = c(3,2))
s <- 0.8
for (i in 12:-5){
  o <- parzen(samples, s**i)
  plot.parzen(o)
}

#e)
parzen.HO <- function(sd,train,test){
  
  sapply(sd, FUN=function(sd){
    p <- parzen(train, sd)
    pred <- predict.parzen(p, newdata=test)
    sum(log(pred))
  })
}

#f)
par(mfrow = c(3,1))

fn <- function(x) parzen.HO(x, samples, samples.new)

curve(fn, xlim=c(0,5))
curve(fn, xlim=c(0.1, 1))
curve(fn, xlim=c(0.5, 0.6))

#g-h)
par(mfrow = c(1,1))
f <- function (x) parzen.HO (x, samples, samples.new)

s.opt <- optimize(f, interval = c(0,5), maximum = TRUE)
logs.opt <- parzen.HO(s.opt$maximum, samples, samples.new)
o <- parzen(samples, s.opt$maximum)
plot.parzen(o)

s.Opt <- Optimize(f, upper = 5, lower = 0, maximum = TRUE)
logs.Opt <- parzen.HO(s.Opt$maximum, samples, samples.new)
o <- parzen(samples, s.Opt$maximum)
plot.parzen(o)