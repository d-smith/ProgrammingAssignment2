# This file contains two functions to enable the use of caching
# to speed up computations involving inverting the same matrix more
# than once.

## makeCacheMatrix creates a matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  cachedInv <- NULL
  set <- function(y) {
    x <<-y
    cachedInv <<-NULL
  }
  get <- function() x
  setinverse <- function(inverse) cachedInv <<- inverse
  getinverse <- function() cachedInv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## cacheSolve returns the inverse of the given matrix, caching the inverse between
## invocations.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("returning cached value")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  return(inv)
}
