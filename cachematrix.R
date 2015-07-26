## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(myx = matrix()) {
  
  invM <- NULL 
  
  set <- function(y) {
    myx <<- y 
    invM <<- NULL 
  }
  
  get <- function() myx 
  
  setInv <- function(inverse) invM <<- inverse 
  getInv <- function() return(invM) 
  return(list(set = set, get = get, setInv = setInv, getInv = getInv))
}


cacheSolve <- function(x, ...) {
  
  answer <- x$getInv() # call from Cache value
  
  if(!is.null(answer)) { # check if the answer is Not NULL (present in Cache)
    
    message("getting cached data") # indicate to user the value is from Cache
    
    return(answer) # return the answer
    
  }
  
  message("new Calculation")
  
  data <- x$get() # retrieve the data
  
  answer <- solve(data, ...) # calculate the inverse Matrix answer
  
  x$setInv(answer) # save the data to Cache
  
  # return the answer
  return(answer)
  
}
