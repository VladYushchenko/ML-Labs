### (a) ###

UNBC <- function(x, nf=length(x)){
    probabilities <- rep(0, nlevels(x[, nf]))
    names(probabilities) <- levels(x[, nf])
    
    class_properties <- list()
    
    for(level in levels(x[, nf])){
        probabilities[level] <- dim(x[x[, nf] == level,])[1] / dim(x)[1]
        
        x_temp <- x[x[, nf] == level,]
        
        # Get maximum, minimum and probability of intervals
        int_mins <- rep(0, length(x) - 1)
        int_maxs <- rep(0, length(x) - 1)
        int_probs <- rep(0, length(x) - 1)
        
        names(int_mins) <- names(x)[-nf]
        names(int_maxs) <- names(x)[-nf]
        names(int_probs) <- names(x)[-nf]
        
        for(i in (1:length(x_temp))[-nf]){
            int_mins[i] <- min(x_temp[, i])
            int_maxs[i] <- max(x_temp[, i])
            int_probs[i] <- 1 / (int_maxs[i] - int_mins[i])
        }
        
        count <- dim(x_temp)[1]
        
        class_properties <- append(class_properties, list(list("int_mins"=int_mins, "int_maxs"=int_maxs, "int_probs"=int_probs)))
    }
    
    names(class_properties) <- levels(x[, nf])
    
    ret <- list("class_probs"=probabilities, "class_properties"=class_properties, "nf"=nf)
    
    class(ret) <- "UNBC"
    invisible(ret)
}

### (b) ###

predict.UNBC <- function(o, newdata){
    as.factor(apply(newdata, MARGIN=1, FUN=function(newdata){
        classif_probs <- rep(0, length(o$class_probs))
        
        names(classif_probs) <- names(o$class_probs)
        
        for(i in 1:length(o$class_probs)){
            classname <- names(o$class_probs)[i]
            class.prob <- o$class_probs[i]
            
            I <- (o$class_properties[[classname]][["int_mins"]] <= newdata & 
                      newdata <= o$class_properties[[classname]][["int_maxs"]])
                #(1 / (o$class_properties[[classname]][["int_maxs"]] - o$class_properties[[classname]][["int_mins"]]))
            
            classif_probs[[classname]] <- 
                class.prob * 
                prod(o$class_properties[[classname]][["int_probs"]]) * all(I)
        }
        
        # Get index of class with maximum value
        index <- which(classif_probs == max(classif_probs))
        
        # Return rejection
        if(sum(classif_probs) == 0){
            "NONE"
        }
        else{
          # Return class
          names(o$class_probs)[index]
        }
    }))
}

### (c) ###

heldout <- function(train, test=train, method=UNBC, nf=length(train), ...){
    o <- method(train, ...)
    classifications <- predict(o, test[-nf])
    # Return error rate
    sum(as.vector(classifications) != as.vector(test[[nf]])) / length(classifications)
}

### (d) ###

print(heldout(iris, iris, UNBC))


### (e) ###

load("segment.rda")

print(heldout(segment.lern, segment.lern, UNBC))
print(heldout(segment.lern, segment.test, UNBC))
print(heldout(segment.test, segment.lern, UNBC))
print(heldout(segment.test, segment.test, UNBC))

### (f) ###

leave1out <- function(x, method=UNBC, nf=length(x)){
    error_rates <- sapply(1:length(x[, 1]), FUN=function(i){
        o <- method(x[-i,])
        
        classification <- predict(o, x[i,-nf])
        
        as.vector(classification) != as.vector(x[[nf]][i])
    })
    
    sum(error_rates) / length(error_rates)
}

### (g) ###

print(leave1out(iris))

print(leave1out(segment.test))

print(leave1out(rbind(iris, iris)))