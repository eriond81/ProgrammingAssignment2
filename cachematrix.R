## Generate an matrix object with the x parameter
## The Object have the follow methods:
## get: get the value of the matrix
## set: set the value of the matrix
## setInverse: sets the inverse of the matrix
## getInverse: gets the inverse of the matrix
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

## Given a matrix X made with the makeCacheMatrix function
## checks if the inverse is stored in the cache, if its in
## cache returns with the stored value if not, then calculate
## the inverse with the solve function, save the value in the cache
## and return the inverse value calculated
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}