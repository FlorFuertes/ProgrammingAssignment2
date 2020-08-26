## Functions below are used to create a special object that stores a matrix
## and caches its inverse.

## This function creates a special "matrix" object, which is a list that sets
## the values of a  matrix, gets them, sets the values of its inverse and gets
## them.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  inverse <- function(inv) m <<- inv
  getinverse <- function() m
  list(set = set, get = get, inverse = inverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" generated with
## the function above. First, it checks if the inverse has already been
## computed. If not, it calculates the inverse of the data and sets the value
## in the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$inverse(m)
  m
}