setwd("~/R/WMM_AG8")
library(class)

### (a) ###

knn.heldout <- function(train, test, k=1){
    classifications <- knn(train[-which(names(train) == "CLASS")], 
                           test[-which(names(train) == "CLASS")], 
                           cl=as.factor(train[["CLASS"]]), 
                           k=k)
    
    # Count number of misclassifications
    sum(ifelse(classifications == test[["CLASS"]], 0, 1)) / length(test[["CLASS"]])
}

### (b) ###

knn.leave1out <- function(data, k=1){
    classifications <- knn.cv(data[-which(names(data) == "CLASS")], cl=as.factor(data[["CLASS"]]), k=k)
    
    # Count number of misclassifications
    sum(ifelse(classifications == data[["CLASS"]], 0, 1)) / length(data[["CLASS"]])
}

### (c) ###

load("germany.rda")

k <- c(1, 2, 3, 5, 8, 11, 14)

table <- matrix(rep(0, length(k) * 5), 5, length(k))

rownames(table) <- 1:5
colnames(table) <- paste("K =", k)

for(i in 1:length(k)){
    v <- knn.heldout(germany.lern, germany.test, k[i])
    
    table[1, i] <- v
    
    cat(v, "\n")
}
cat("\n")

### (d) ###

for(i in 1:length(k)){
    v <- knn.heldout(germany.test, germany.lern, k[i])
    
    table[2, i] <- v
    
    cat(v, "\n")
}
cat("\n")
cat("\n")
### (e) ###

for(i in 1:length(k)){
    v <- knn.leave1out(rbind(germany.lern, germany.test), k[i])
    
    table[3, i] <- v
    
    cat(v, "\n")
}

### (f) ###

for(j in 1:3){
    for(i in 1:length(k)){
        v <- knn.leave1out(rbind(germany.lern, germany.test), k[i])
        
        table[2 + j, i] <- v
        
        cat(v, "\n")
    }
    cat("\n")
}
### (g) ###

load("letter.rda")

len <- (2:15)^4 / 4

res.g <- rep(0, length(len))

for(i in 1:length(len)){
    res.g[i] <- knn.heldout(letter.lern[1:len[i],], letter.test, k = 1)
}

res.g

### (h) ###

load("vehicle.rda")

res.h <- rep(0, 19)

for(i in 1:18){
    res.h[i] <- knn.leave1out(rbind(vehicle.lern[,-i], vehicle.test[,-i]), k = 1)
}

res.h[19] <- knn.leave1out(rbind(vehicle.lern, vehicle.test), k = 1)

res.h

library(htmlTable)

htmlTable(table, file="R/table_cdef.html")

res.g <- matrix(res.g, 1, length(res.g), byrow=T)
colnames(res.g) <- len

res.h <- matrix(res.h, length(res.h), 1)
rownames(res.h) <- c(names(vehicle.lern)[1:length(res.h) - 1], "NONE")

htmlTable(res.g, file="R/table_g.html", cgroup="Length of train data", n.cgroup=length(res.g))
htmlTable(res.h, file="R/table_h.html", rgroup="Left out feature", n.rgroup=length(res.h))

