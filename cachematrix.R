## These two functions let you compute the inverse of a given matrix
## taking care about the "newness" of it: if the matrix is already
## known, its inverse is loaded from cache where it was previously
## stored.
## What is going on is seen if you use the "set" function to
## initialize a new matrix: when you call "set(y)" you assign
## the y matrix to x in the global environment and put m to NULL
## in the global environment as well, so that the cacheSolve 
## function will read those new values.

## mCM function takes a matrix and defines a bunch of functions
## to work with it, returning a list

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## cS function has to be used together with mCM, like this:
## cacheSolve(makeCacheMatrix(x)), and will return the inverse of 
## matrix x, reading it from cache if already computed

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}