library(Matrix)
##Create cache matrix
makeMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
##invert matrix
cacheInvert <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {       ##look for cache data
    message("getting cached data")
    return(inv)
  }
  m.data <- x$get()
  inv <- solve(m.data, ...)
  x$setinverse(inv)
  
  return(inv)
}
