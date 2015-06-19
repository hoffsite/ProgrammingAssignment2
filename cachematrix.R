## First function creates an object
## that holds a cached of data (matrix and inverted matrix)
## and defines functions that may be used
## to store and retrieve cache.
##
## Second function will control computation of inverse to 
## occur only when necessary and provide inverse on demand.

## First Function. Make an object that contains
##   DATA
##   x: the matrix to be inverted,
##   m: its inverted matrix and
##   FUNCTIONS
##   set: cached matrix (and null cached inverse)
## , get: cached matrix
## , setInverse: will cache inverse matrix
## , getInverse: will return cached inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Second Function.
## x is the object created by makeCacheMatrix.
## Retrieve the already defined inverted matrix.
## If not defined, compute it, store it in the cache, and return it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
    if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
