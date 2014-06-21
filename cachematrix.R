## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix takes a matrix and converts it to a "cache matrix" capable of caching it's own
## inverse.  Returns a list of functions to implement this.
## The get and set functions can be used to return or set the matrix.  
## getinverse and setinverse are intended for use by the cacheSolve function to utilze the cache
## to store the inverse so that it doesn't need to be solved again.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
      getinverse = getinverse)
}


## cacheSolve takes  a cacheMatrix and solves for the inverse of it, first checking to see if the
## inverse has already been solved and is available in cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
