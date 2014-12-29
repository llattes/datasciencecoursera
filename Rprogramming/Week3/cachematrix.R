## This set of functions is intended for calculating and caching the inverse
## of a matrix. Matrix inversion could be a costly computation and there may
## be some benefit to caching the inverse of a matrix rather than compute it
## every time it is needed.

## This function creates a special "matrix" object that can cache its inverse.
## To avoid the default behaviour described in the assignment details:
## "For this assignment, assume that the matrix supplied is always invertible."
## You can set the optional paramenter 'enforceInvertible' to TRUE and the
## makeCacheMatrix function will check for invertibility before creating the
## cacheMatrix object to prevent runtime errors while executing cacheSolve.
makeCacheMatrix <- function(originalMatrix = matrix(),
                            enforceInvertible = FALSE) {
  # Optional check of matrix invertibility before making the cache matrix.
  if (enforceInvertible == TRUE) {
    if (dim(originalMatrix)[1] != dim(originalMatrix)[2]) {
      message("The matrix ", originalMatrix, " is not invertible since it is ",
              "not square.")
      return()
    }
    if (det(originalMatrix) == 0) {
      message("The matrix ", originalMatrix, " is not invertible since its ",
              "determinant is exactly equal to zero.")
      return()
    }
  }
  inverse <- NULL
  set <- function(newMatrix) {
    originalMatrix <<- newMatrix
    inverse <<- NULL
  }
  get <- function() originalMatrix
  setInverse <- function(theInverse) inverse <<- theInverse
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and
## the matrix has not changed), then the cachesolve should retrieve the
## inverse from the cache.
cacheSolve <- function(cacheMatrix, ...) {
  # Check for nullity before making any operations.
  if (is.null(cacheMatrix)) {
    message("The cacheMatrix object cannot be NULL.")
    return()
  }
  inverse <- cacheMatrix$getInverse()
  if (!is.null(inverse)) {
    message("Getting cached inverse of ", cacheMatrix$get())
    return(inverse)
  }
  data <- cacheMatrix$get()
  inverse <- solve(data, ...)
  cacheMatrix$setInverse(inverse)
  inverse
}
