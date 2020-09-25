## A set of functions that deal with caching the inverse of a matrix

## This function makes a special matrix object that caches its inverse
makeCacheMatrix <- function(x = matrix()) {
  ## Initialize the inverse variable
  a <- NULL
  
  ## Method to set the matrix
  set <- function(theMatrix) {
    x <<- theMatrix
    a <<- NULL
  }
  
  ## Method to get the matrix
  get <- function() x

  ## Method to set the inverse of the matrix
  setInverse <- function(inverse) a <<- inverse
  ## Method to get the inverse of the matrix
  getInverse <- function() a
  
  ## Returning the list of methods
  list(set = set, get = get,
    setInverse = setInverse,
    getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  a <- x$getInverse()
  
  ## Return the inverse if its already set
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## get the matrix from the x object
  data <- x$get()
  
  ## Calculate the inverse using matrix multiplication
  m <- solve(data) %*% data
  ## Set the inverse to the object
  x$setInverse(m)
  ## Return the matrix
  m
}
