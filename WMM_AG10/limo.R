
### (a) ###

load("limo.rda")

### (b) ###

polyfun <- function(x, a){
    sapply(x, FUN=function(x){
        sum(x^(0:(length(a) - 1)) * a)
    })
}

### (c) ###

polyfit <- function(x, n) {
    if(n > 0)
        form <- as.formula(paste("target~", paste("I(source^", 1:n, ")", collapse="+")))
    else
        form <- as.formula(paste("target~1"))
    print(form)
    lm(form, data=x)
}

### (d) ###

polyfits <- function(x, order, plot=F){
    ret <- sapply(order, FUN=function(n){
        polyfit(x, n)
    }, simplify = F)
    
    if(plot){
        aics <- sapply(ret, FUN=AIC)
        bics <- sapply(ret, FUN=BIC)
        
        plot(order, aics, col="red", bg="red", pch=21, main="Informationskriterien AIC (rot) und BIC (blau)")
        points(order, bics, col="blue", bg="blue", pch=21)
    }
    
    ret
}

poly <- polyfits(sample2D, 0:11, plot=T)

### (e) ###

par(ask=T)

layout(matrix(1:4, 2, 2, byrow=T))

for(i in 1:12){
    #(1)
    plot(sample2D, col="red", bg="red", pch=21, main=paste("Polynomgrad", i - 1))
    #(2)
    points(sort(sample2D$source), polyfun(sort(sample2D$source), poly[[i]]$coefficients), col="blue", type="l", lwd=3)
    #(3)
    mtext(paste(signif(poly[[i]]$coefficients, 2), collapse=", "))
    #(4)
    mtext(paste("AIC = ", round(AIC(poly[[i]]), 1)), side=4)
}

layout(matrix(1))