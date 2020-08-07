## Below are shown a set of functions that print an inverse of a a given matrix.
## functions do

## This function (makecacheMatrix) creates a special “matrixvector”, which is really a list containing a function to

## - set the values of the matrix
## - get the values of the matrix
## - set the inverse of this matrix
## - get the inverse of this matrix

## This function (makeCacheMatrix) creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
  y <- NULL
  set <- function(z){
    x <<- z
    y <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) y <<- inverse
  getInverse <- function() y
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
 
  y <- x$getInverse()
  if(!is.null(y)){
    message("getting cached data")
    return(y)
  }
  data <- x$get()
  y <- solve(data,...)
  x$setInverse(y)
  y
}
