## Date         : August 8 2014
## Version      : 1.2
## Desccription : Pair of functions for calculating matrix inverse.
##                These methods make use or R's caching capabilities to improve performance.

## Creates a special cacheable matrix in the environment.

makeCacheMatrix <- function(x = matrix()) {
  
  ##Initialize inverse to NULL
  m <- NULL
  
  ##Setter method for matrix
  set <- function(matix) {
    x <<- matrix
    m <<- NULL
  }
  
  ##Getter method for matrix
  get <- function() x
  
  
  ##Setter method for inverse
  setInverse <- function(inverse) m <<- inverse
  
  ##Getter method for inverse
  getInverse <- function() m
  
  ##List of methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Returns inverse of the matrix. Uses cached copy incase the matrix is as it is.
cacheSolve <- function(x, ...) {
  
  ##Get inverse of x
  m <- x$getInverse()
  
  ##Check if the inverse is already available and use.
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ##Get matrix
  data <- x$get()
  
  ##Calculate inverse
  m <- solve(data) %*% data
  
  ##Set inverse in the object
  x$setInverse(m)
  
  ##Return matrix
  m
  
}
