## Matrix inversion with results caching
## makeCacheMatrix creates special "matrix" that can cache its inverse.
## cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  ## Return listof functions (set, get, getinv, setinv )
  ## set(x) sets matrix
  ## get(x) gets current matrix
  ## getinv() return cahed inverse matrix
  ## setinv(x) saved inverse matrix to the cache
  i <- NULL
  set <- function(new_x) {
    x <<- new_x
    i <<- NULL
  }
  get <- function() x
  setinv <- function(new_i) i <<- new_i
  getinv <- function() i
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function computes the inverse of the special "matrix"
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## x is the value returned by makeCacheMatrix
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached result")
    return (i)
  }
  m <- x$get()
  i <- solve(m, ...)
  x$setinv(i)
  i
}
