
# a set of functions to set and retrieve matrix
makeCacheMatrix <- function(x = matrix()) {
  
  inMtx <- NULL #initiate inMtx to NULL
 
  set <- function(y) {
    x <<- y #overwrites the x value to y in parent environment
    inMtx <<- NULL #set NULL value in parent environment
  }
  
  #return x value
  get <- function() x 
  
  #
  setInv <- function(inverse) inMtx <<- inverse #save inverse value as cache
  getInv <- function() return(inMtx) #return cached value
  
  return(list(set = set, get = get, setInv = setInv, getInv = getInv))
}


cacheSolve <- function(x, ...) {
  
  ans <- x$getInv() 
 
   if(!is.null(ans)) { #check if it is not NULL
    
    message("getting cached data") 
    return(ans) #return cached value of ans
  }
  
  message("new Calculation") #indicate it is not cached
  
  data <- x$get()  #fetch data
  ans <- solve(data, ...) #inverse data
  
  x$setInv(ans) #save value as cache
  return(ans)
  
}

#sample running code
#> source("cachematrix.R")
#> myM <- matrix(c(4,2,7,6),2)
#> myM
#[,1] [,2]
#[1,]    4    7
#[2,]    2    6
#> test <- makeCacheMatrix(myM)
#> cacheSolve(test)
#new Calculation
#[,1] [,2]
#[1,]  0.6 -0.7
#[2,] -0.2  0.4
#> cacheSolve(test)
#getting cached data
#[,1] [,2]
#[1,]  0.6 -0.7
#[2,] -0.2  0.4
#> 