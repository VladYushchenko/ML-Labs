
setwd("~/R/WMM_AG6")
#EGST	setwd("C:\\Users\\Matthias\\Dropbox\\Programmierung\\R\\Uebungen_Werkzeuge_ME_ML\\6")

#a-c)
data("iris")
load('pca.rda')	#EGST

plot.lfd <- function(x, subset=(1:length(x))[-nf], nf=length(x), ...) {
  #EGST	besser:  subset=(1:length(x))[-nf]	wegen nf!=length(x)
  #
  # x       - data
  # subset  - vector of included features (default - all non factors)
  # nf      - indecies of included classfactor
  # main    - plot title
  #
  
  # select elements in classfactor(nf) with specified feature(subset)
  plot(x[,subset], col=x[[nf]], ...)
  #EGST	besser:  rainbow(nlevels(x[[nf]]))
}

layout(matrix(1))

plot.lfd(iris, subset = 1:4, main="Iris 1:4")
plot.lfd(iris, subset = 1:3, main="Iris 1:3")
plot.lfd(iris, subset = 1:2, main="Iris 1:2")
plot.lfd(iris, subset = 1:1, main="Iris 1:1")

#plot.lfd (PCA (iris, n=4))
iris.part <- iris [iris$Species != 'setosa',]

# d)
PCA <- function(x, train=x,
                n=length(x)-1,	#EGST	default: alle PC Merkmale
                center=TRUE, scale=TRUE )
{
  #
  # x       - data
  # train   - data for stats
  # center  - centering of data
  # scale   - scaling of data
  
  data <-
    
    x[-length(x)]	#EGST	alle Daten transformieren!
  covX <- cov(train
              #EGST	[1:n])
              [-length(x)])	#EGST	komplette Kovarianzmatrix der Lerndaten
  eigenPairs <- eigen(covX)
  eigenMat <-
    #EGST	eigenPairs$vectors
    eigenPairs$vectors[,1:n]	#EGST	erste n Achsen!
  
  # permutate vectors in eigenMatrix according to eigenvalues
  U = eigenMat #eigenMat[,order(eigenPairs$values, decreasing = T)]
  lambda <-
    #EGST	eigenPairs$values #sort(eigenPairs$values, decreasing = T)
    eigenPairs$values[1:n]	#EGST	erste n Achsen
  
  if (center) {
    mu <- apply(train
                #EGST		[1:n],
                [-length(x)],	#EGST	s.o.
                2, mean)
    data <- sweep(data, 2, mu, FUN = "-")
  }
  if (scale) {
    powD <-
      diag(lambda**(-0.5))
    U <- U %*% powD
  }
  res <- cbind(data.matrix(data) %*% U, x[length(x)])
}

# e)
par(mfrow = c(2,2))
plot.lfd (PCA (iris, n=2), main="PCA(Iris)")
plot.lfd (PCA (iris, n=2, center = FALSE), main="PCA(Iris), center == FALSE")
plot.lfd (PCA (iris, n=2, scale =  FALSE), main="PCA(Iris), scale == FALSE")
plot.lfd (PCA (iris, n=2, center = FALSE, scale =  FALSE), main="PCA(Iris), center == FALSE, scale == FALSE")

# f)
par(mfrow = c(2,2))
plot.lfd (PCA (iris, n=2), main = "x=iris, train=iris")
plot.lfd (PCA (iris, train = iris.part, n=2), main = "x=iris, train=iris.part")
plot.lfd (PCA (iris.part, train=iris, n=2), main = "x=iris.part, train=iris")
plot.lfd (PCA (iris.part, train=iris.part, n=2), main = "x=iris.part, train=iris.part")

# g)

show.data <- function(x, main = ""){
  par(mfrow = c(2,2))
  plot.lfd(x, subset = 1:2, main = paste(main, ""))
  plot.lfd(x, subset= (length(x)-2):(length(x)-1), main = main)
  plot.lfd(PCA (x, n=2), main = main)
  plot.lfd(PCA(x)[length(x)-2:0],	main = main)
}

datasets <- list(Iris=iris, ADIDAS=ADIDAS, Diabetes=diabetes, DNA=dna, FIAT=FIAT, Heart=heart, Vehicle=vehicle, Watermark=watermark)

for(i in seq_along(datasets)){
  show.data(datasets[[i]], names(datasets)[i])
}

