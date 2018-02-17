# Find below two Functions that cache the inverse of a
# matrix.

## The first, makeCacheMatrix, is a function that creates
## a matrix to store a list containing the following
## functions:
##
## - setMatrix: Sets the value of a matrix.
## - getMatrix: Gets the value of a matrix.
## - cacheInverse: Sets the inverse of the matrix.
## - getInverse: Gets the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  setMatrix <- function(newValue) {
    x <<- newValue
    cache <<- NULL
  }
  getMatrix <- function() x
  cacheInverse <- function(solve) {
    cache <<- solve
  }
  getInverse <- function() cache
  list(
    setMatrix = setMatrix, 
    getMatrix = getMatrix, 
    cacheInverse = cacheInverse, 
    getInverse = getInverse
    )
}

## The second, cacheSolve, calculates the inverse of the matrix
## created and cached with the first function, makeCacheMatrix;
## or reuses cached result if it is already available.

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
      message("Getting cached data.")
      return(inverse)
    }
    data <- x$getMatrix()
    inverse <- solve(data)
    x$cacheInverse(inverse)
    inverse
}
