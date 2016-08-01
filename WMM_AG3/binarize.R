#setwd("~/WMM_AG3")
load("binarize.rda")

binarize <- function (x, method="fixed", threshold=0.5) {
  if(method == "mean") {
    threshold = mean(x)
  }
  else if(method == "median") {
    threshold = median(x)
  }
  else if(method == "2-means") {
    threshold = kmeans(x, c=2)
    threshold = mean(threshold[[2]])
  }
  ind <- which(x <= threshold)
  x[ind] = 0
  ind <- which(x != 0)
  x[ind] = 1
  return (x)
}

# b)
par(mfrow = c(2, 2))
image(algae)
hist(algae, breaks = 24)
alga <- algae[50:100,10:60]
image(alga)
hist(alga)

# c)
par(mfrow = c(3, 3))
thres <- seq(0.2,0.6, length = 9)
for (i in thres) {
out <- binarize(alga, threshold = i)
image(out)
}

# d)
par(mfrow = c(2, 2))
data <- list("alga"=alga, "algae"=algae, "zdfneo"=zdfneo)

# processing data
for (i in 1:length(data)){
  image(binarize(data[[i]]), xlab="Threshold = 0.5")
  image(binarize(data[[i]], method="mean"), xlab="mean")
  image(binarize(data[[i]], method="median"), xlab="median")
  image(binarize(data[[i]], method="2-means"), xlab="2-means")
}

