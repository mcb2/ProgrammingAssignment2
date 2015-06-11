## These functions are used to store a matrix and to calculate and cache its inverse.
##
## makeCacheMatrix creates an object with functions for storing and retrieving a matrix
## and its inverse.
## set - stores the value of the matrix. (The matrix can also be stored when the 
##       makeCacheMatrix function is initially called if it is provided as an argument)
## get - returns the value of the matrix
## setsolve - stores the inverse of the matrix. Should only be called by the
##            cacheSolve function.
## getsolve - returns the inverse of the matrix. Should only be called by the
##            cacheSolve function.
##
## cacheSolve calculates and caches the inverse of the matrix associated with the 
## object created with makeCacheMatrix.  If the inverse was calculated previously 
## a cached copy is returned.
##
## Example:
# tm <- matrix(c(2,2,3,2), nrow = 2, ncol = 2)
# t <- makeCacheMatrix(tm)
# t$get()
# #[,1] [,2]
# #[1,]    2    3
# #[2,]    2    2
# cacheSolve(t)
# #[,1] [,2]
# #[1,]   -1  1.5
# #[2,]    1 -1.0
# cacheSolve(t)
# #getting cached data
# #[,1] [,2]
# #[1,]   -1  1.5
# #[2,]    1 -1.0
 

## Create a list of functions that will be used to store and retrieve a matrix
## and its inverse

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  ## set the value of the matrix
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  ## return the matrix (previously provided with set function or when makeCacheMatrix
  ## was called)
  get <- function() x
  ## set the value of the inverse of the matrix
  setsolve <- function(solve) s <<- solve
  ## return the value of the inverse of the matrix
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Calculate and return the inverse of the provided matrix created with makeCacheMatrix

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
