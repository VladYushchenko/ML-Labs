
polyterms <- function(v,d){
  if (d == 0) return (as.formula("y~1"))
  for (i in 0:d){
    form <- ""
    if (i == 0) 
      result <- "y~1 + "
    else{
      coef.matr <- expo(length(v), d)
      forms <- oneterm(v, coef.matr)
      form <- paste(form, forms, collapse = "+")
    }
  }
  result <- as.formula(paste(result, form))
  return (result)
}

expo <- function(n, d){
  if(n == 1){
    return(matrix(d))
  }
  
  v <- NULL
  
  for(i in d:0){
    
    e <- expo(n - 1, i)
    
    if(!is.null(v))
      v <- rbind(v, cbind(rep(d - i, nrow(e)), e))
    else
      v <- cbind(rep(d - i, nrow(e)), e)
  }
  
  v
}

oneterm <- function(v,ex){
  stopifnot(length(v) == ncol(ex))
  
  f <- function (x) {
    paste("I(", paste(v, rep("^", length(x)), x, collapse='*', sep=""), ")", sep="")
  }
  
  temp <- apply(ex, MARGIN = 1, f)
  
  answer <- paste(temp, collapse = "+", sep = " ")
}

QMK <- function(x, d=1){
  form <- polyterms(names(x[-length(x)]), d)
  termcount <- length(attr(terms.formula(form), "term.labels"))
  
  x <- cbind(x, y=as.numeric(x[["CLASS"]]))
  classified <- lm(form, data=x)
  
  object <- list(
    "lm"=classified, 
    levels=levels(x[["CLASS"]]),
    "n.terms"=termcount)
  class(object) <- "QMK"
  
  return(object)
}

predict.QMK <- function(o, newdata){
  v <- as.factor(round(predict(o$lm, newdata), 0))
  #print(levels(v))
  #levels(v) <- o$levels
  
  v
}

heldout.plus <- function (train,test,method=QMK,...) {
  o <- method(train, ...)
  classifications.train <- predict(o, train)
  classifications.test <- predict(o, test)
  
  
  errs.train <- sum(as.vector(classifications.train) != as.vector(train[["CLASS"]])) / length(classifications.train)
  errs.test <- sum(as.vector(classifications.test) != as.vector(test[["CLASS"]])) / length(classifications.test)
  
  n.terms.train <- classifications.train
  n.terms.test <- classifications.test
  n.terms <- o$n.terms
  
  return (list(errors_train = errs.train, errors_test=errs.test, 
               terms_train = n.terms.train, terms_test = n.terms.test,
               number_terms = n.terms))
}


error.rates <- function(train,test,...){
  x <- 0:3
  errs.train <- rep(0,4)
  errs.test <- rep(0,4)
  n.polyterms <- rep(0,4)
  for (i in x){
    plot.data <- heldout.plus(train,test, d=i)
    errs.train[i] <- plot.data$errors_train
    errs.test[i] <- plot.data$errors_test
    n.polyterms[i] <- plot.data$number_terms
  }
  
  data.table <- rbind(errs.train,  errs.test, n.polyterms)
  rownames(data.table) <- c('Error.train', 'Error.test', 'N.polytrerms')
  colnames(data.table) <- c(paste('Pol.grade', x))
  
  print(as.table(data.table))
  
  plot(x, errs.train, type = "l", main="Errors", col="red")
  lines(x, errs.test, type = "l", col="blue")
}

### 
data.list = list("vehicle", "heart",  "australia", "diabetes", "segment")


for (i in 1:length(data.list))
  load (paste(data.list[[i]],".rda", sep=""))

error.rates(australia.lern, australia.test)
error.rates(heart.lern, heart.test)
error.rates(diabetes.lern, diabetes.test)
error.rates(vehicle.lern, vehicle.test)
error.rates(segment.lern, segment.test)
