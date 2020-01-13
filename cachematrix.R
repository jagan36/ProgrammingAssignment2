## makeCacheMatrix function
## This function makes a cache of matrix
## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  invrs <- NULL
  set <- function(y) {
    x <<- y
    invrs <<- NULL
  }
  get <- function() x
  set_invrs <- function(inverse) inv <<- inverse
  get_invrs <- function() invrs
  list(set = set,
       get = get,
       set_invrs = set_invrs,
       get_invrs = get_invrs)
}

## cacheSolve function:
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), the CacheSolve function 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  invrs <- x$get_invrs()
  if (!is.null(invrs)) {
    message("getting cached data")
    return(invrs)
  }
  mtrx <- x$get()
  invrs <- solve(mtrx, ...)
  x$set_invrs(invrs)
  inv
}