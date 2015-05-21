## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse
## of a matrix rather than compute it repeatedly. These two functions create a special "matrix" object 
## that can cache its inverse if it's caluculated by cacheSolve() function.
## Here we assume that matrix supplied is always invertible.

## This function creates a special "matrix" object that can cache its inverse.
## Symbol "x" is created in the scope of the matrix and then other functions, defined inside of makeCacheMatrix,
## have access to it as a free variable, thus using environment of this function as a storage for the matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inversehas already been calculated. If so, it gets the inverse from
## the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value
## of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x',
        ## where 'x' can't be a matrix but should be special cachable matrix object instead
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
