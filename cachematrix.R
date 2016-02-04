## Programming Assignment 2: Lexical Scoping:
## Caching the inverse of a square Matrix

## This function is designed to cache the inverse of a square Matrix 
## It should save computation time by looking at the cache if the operation
## was already done and loading the result from there, or, in case it isn´t 
## previously done by creating the inverse of a given matrix.

## This function should work with square matrices,
## since the operator to calculate the inverse is solve(X), X is a matrix
## if the matrix is not square then the operator should be ginv(X),
## from the MASS library.

## Given a a square invertible matrix (X)
## The first function "makeCacheMatrix()" creates a list containing
## a fuction to:
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse
## This list is used as the input to the fuction "cacheSolve()"

makeCacheMatrix <- function(x = matrix()) {
      
      inv = NULL
      set = function(y) {
            x <<- y
            inv <<- NULL
            # the operator "<<-" assign a value to X (in this case, "y")
            # looking in an environment different from the current one.
      }
      get = function() x
      setinv = function(inverse) inv <<- inverse 
      getinv = function() inv
      list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function, "cacheSolve(x)" calculates the inverse of a square matrix (X)
## First, checks if the inverse of X has already been calculated.
## If so, gets the inverse of X fromthe cache and skips computation
## Otherwise calculates the inverse of X and sets the inverse in the cache,
## with the "setinv()" function.

cacheSolve <- function(x, ...) {
      
      inv = x$getinv()
      
      # if the inverse has already been calculated
      if (!is.null(inv)){
            # get it from the cache and skips the computation. 
            message("getting cached data")
            return(inv)
      }
      
      # otherwise, calculates the inverse 
      Mdat = x$get()
      inv = solve(Mdat, ...)
      
      # sets the value of the inverse in the cache.
      x$setinv(inv)
      
      return(inv)
}
