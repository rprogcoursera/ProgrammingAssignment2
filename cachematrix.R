## makeCacheMatrix(m) takes matrix m and creates an object that
## enables cacheSolve to calculate and cache the inverse of m.

## makeCacheMatrix(m) takes matrix m and creates and associates 
## m with a list of utility functions for getting and setting 
## the underlying data as well as the inverse of m.
## A makeCacheMatrix object is to be used in conjection with 
## cacheSolve to calculate and cache the inverse of m.
## Notice that changing the stored matrix with set will clear 
## the cached inverse.
makeCacheMatrix <- function(m = matrix()) {
  inv <- NULL
  set <- function(m2) {
    m <<- m2
    inv <<- NULL
  }
  get <- function() m
  setinv <- function(newinv) inv <<- newinv
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve(mcm, ...) takes a makeCacheMatrix object,
## calculates the inverse for the matrix stored in mcm,
## and caches the result (in mcm). If the matrix has 
## not changed (via mcm$set) since the last time cacheSolve
## is called and a valid inverse is already cached, then 
## cacheSolve returns the cached result instead of 
## re-computing the inverse.
cacheSolve <- function(mcm, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- mcm$getinv()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  data <- mcm$get()
  inv <- solve(data, ...)
  mcm$setinv(inv)
  inv
}
