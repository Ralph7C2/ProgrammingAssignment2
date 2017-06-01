## cacheMatrix is a structure that contains a matrix and a cached
## inverse(once calculated the first time), until the matrix is replaced.

## makeCacheMatrix constructs an object capable of storing a matrix
## and caching it's inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve checks for a cached inverse and returns it if present, otherwise
## solves the inverse and caches it.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
      message("Getting cached inverse")
      return(inv)
    }
    data<-x$get()
    inv <- solve(data)
    x$setInverse(inv)
    inv
}
