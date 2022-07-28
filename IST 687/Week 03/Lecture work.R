mtcars
mtcars[1,1]
summary(mtcars)
str(mtcars)
colnames(mtcars)
rnames <- rownames(mtcars)
rnames[20]

bestMPG <- function(){
  index <- which.max(mtcars$mpg)
  car <- mtcars[index,]
  return(car)
}

bestMPG()

bestMPGName <- function(){
  index <- which.max(mtcars$mpg)
  rnames <- rownames(mtcars)
  car <- rnames[index]
  return(car)
}

bestMPGName()

col.index <- colnames(mtcars) =="mpg"

bestWithindex <- function(col.index){
  index <- which.max(mtcars[,col.index])
  rnames <- rownames(mtcars)
  car <- rnames[index]
  return(car)
}

bestWithindex(1)
which.max(mtcars[,1])
mtcars[,1]

best <- function(col.index){
  col.index <- which(colnames(mtcars)==col.name)
  index <- which.max(mtcars[,col.index])
  rnames <- rownames(mtcars)
  car <- rnames[index]
  return(car)
}

best("mpg")
