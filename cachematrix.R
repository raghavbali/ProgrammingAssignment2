## Date         : August 8 2014
## Version      : 1.0
## Desccription : Pair of functions for calculating matrix inverse.
##                These methods make use or R's caching capabilities to improve performance.

## Creates a special cacheable matrix in the environment.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(matix) {
    x <<- matrix
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Returns inverse of the matrix. Uses cached copy incase the matrix is as it is.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- inverse(data, ...)
  x$setInverse(m)
  m
}
