## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## MakeVector creates a special "vector", which is really a list containining functions.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  ## 1. set the value of matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  ## 2. get the value of matrix
  get <- function() x
  
  ## 3. set the value of inverse 
  setinverse <- function(solve) i <<- solve
  
  ## 4. get the value of inverse
  getinverse <- function() i
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function calculates the inverse matrix with the above function. 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## It first checks to see if the mean has already been calculated. 
  i <- x$getinverse()
  
  if(!is.null(i)) {
      ## If so, it gets the inverse from the cache and skips the computation. 
      message("getting cached data")
      return(i)
  }
  
  ## Otherwise, it calculates the inverse of the data 
  data <- x$get()
  i <- solve(data, ...)
  
  ## It then sets the value of the inverse in the cache via the setinverse function.
  x$setinverse(i)
  i
}
