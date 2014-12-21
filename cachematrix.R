## makeCacheMatrix creates a special matrix that will cache its inverse matrix.
## cacheSolve returns the inverse of the given special matrix.

## makeCacheMatrix returns a list of operations that can be used
## to get and set a matrix, and
## to get and set the matrix's inverse matrix.
## x must be a square, invertible matrix.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(i) inverse <<- i
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve returns the inverse of the given special matrix x,
## where x is created by makeCacheMatrix.
## If the inverse has already been calculated, then
## the cached inverse is returned;
## else the inverse is calculated, cached, and returned
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
