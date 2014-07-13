## This file contains two functions 'makeCacheMatrix' and 'cacheSolve'

## makeCacheMatrix takes an ordinary matrix and extends it to allow 
## the inverse to be stored in a cache

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve retrns the inverse of a matrix
## It returns a cached value if one is stored, if not it:
## calculates the inverse, stores it in the cache, and then returns it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
