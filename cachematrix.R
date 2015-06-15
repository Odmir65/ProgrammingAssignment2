## *****************************************************
## COURSERA - R PROGRAMMING - WEEK 3
## Odmir Aguiar
## This function creates a special "matrix" object that can cache its inverse.
## *****************************************************
## create a test matrix
## M = matrix(rnorm(36), nrow = 6, ncol = 6, 1:10)
## *****************************************************
makeCacheMatrix <- function(x = matrix()) {
  # initialize inverse matrix with NULL
  INVx <- NULL
  # save W.E. the matrix
  set <- function(y) {
    x <<- y
    INVx <<- NULL
  }
  # save W.E. the inverse matrix
  setinv <- function(solve) INVx <<- solve
  # load W.E. the matrix
  get <- function() x
  # load W.E. the inverse matrix
  getinv <- function() INVx
  # output
  return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}
## *****************************************************
## COURSERA - R PROGRAMMING - WEEK 3
## Odmir Aguiar
## This function computes the inverse of the returned "special matrix" returned by
##  makeCacheMatrix.R (not the original matrix!)
## *****************************************************
cacheSolve <- function(x,...) {
  # load InvX
  INVx <- x$getinv()
  # check if INVx exist and return it if yes
  if(!is.null(INVx)) {
    message("getting cached data")
    return(INVx)
  }
  # load W.E. the original matrix
  X <- x$get()
  # Is it Square?
  if (nrow(X) != ncol(X)) {
    message("The matriz is not square")
    return(NA)
  }
  # Is the determinant is equal 0?
  if (det(X) == 0) {
    message("The matriz is not inversible, det(x) = 0")
    return(NA)
  }
  # Make InvX
  INVx <- solve(X,...)
  # save W.E. the inverse matrix
  x$setinv(INVx)
  # output
  return(INVx)
}