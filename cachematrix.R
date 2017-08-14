## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Sugath Mudali, R Programming - Johns Hopskins University, Coursera
## Date: 14/08/2017

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # Returns the matrix
  get <- function() {
    x
  }
  
  # Sets the inverse of the matrix
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  # Returns the inverse
  getInverse <- function() {
    inv
  }
  
  # List of functions available
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and
## the matrix has not changed), then the cachesolve should retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  
  # It's null when we call it first time
  if (!is.null(inv)) {
    message("returning the cached inverse")
    return(inv)
  }
  # Get the matrix, inverse it using solve and set it
  inv <- solve(x$get(), ...)
  x$setInverse(inv)
  inv
}

## ---------------Test the program------------------------
#m <- matrix(c(
#          5, 1, 0,
#          3,-1, 2,
#          4, 0,-1), nrow=3, byrow=TRUE)
# If true, inverse exists (ref: https://cran.r-project.org/web/packages/matlib/vignettes/inv-ex1.html)
#print(det(m) != 0)
#cache_matrix <- makeCacheMatrix(m)
#print(cacheSolve(cache_matrix))