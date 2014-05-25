## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates two function that cached the inversion of a matrix

## Write a short comment describing this function

## This function calculate and store the inverse of a matrix in the cache.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL   
  mset <- function(y) {
    x <<- y
    inv <<- NULL
  }
  mget <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(mset = mset, mget = mget,
       setinv = setinv,
       getinv = getinv)

}


## This function checks if the inverse of a matrix was already stored in the cache
## if the inverse wasn't stored in the cache, it calculates the inverse of the matrix

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$mget()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
