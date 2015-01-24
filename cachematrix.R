## This file contains two R functions.
## makeCacheMatrix: to wrap a matrix into an object.
## cacheSolve:      to calculate the inverse of the matrix created by mackeCacheMatrix.
## Calling cacheSolve repeatedly will return a cached value for the inverse without calculating it.
## To reuse the matrix object returned my makeCacheMatrix call its "set" method with a new matrix.
## The "set" method will reset the cached inverse of the matrix, so that cacheSolve will calculate
## and cache the inverse first time cacheSolve is called.
## Example:
## B <- matrix(c(1,0,0,0,1,0,0,0,1),nrow=3,ncol=3)
## C <- makeCacheMatrix(B)
## cacheSolve(C)

## This function creates a special "matrix" object that can cache its inverse.
## Object methods:
##  set: sets the matrix and resets the inverse
##  get: gets the matrix
##  getinverse: gets the cached inverse of the matrix
##  setinverse: sets the inverse of the matrix into the cache
makeCacheMatrix <- function(x = matrix()) {
  ## assume that the matrix supplied is always invertible
  
  ## cached mean is not calculated at initialization
  i <- NULL
  
  ## overwrite matrix and clear the cached inverse
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## return matrix
  get <- function() x
  
  ## cache the inverse of the matrix
  setinverse <- function(inverse) i <<- inverse
  
  ## get the cached inverse of the matrix
  getinverse <- function() i
  
  ## return a list of named methods
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), then this function
## will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## get cached inverse of the matrix
  i <- x$getinverse()
  
  ## check whether inverse was cached before
  if(!is.null(i)) {
    ## yes, the inverse was calculated before
    ## return the cached inverse
    return(i)
  }
  
  ## no, the inverse wasn't cached before.

  ## calculate the inverse now and cache it
  
  ## get the matrix
  data <- x$get()

  ## calculate the inverse
  i <- solve(data, ...)
  
  ## cache the inverse
  x$setinverse(i)
  
  ## return the inverse
  i
}
