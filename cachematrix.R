## Matrix inversion is usually a costly computation and there may be benefit
## to caching the inverse of a matrix rather than compute it repeatedly.
## If you have a large square matrix, instead of calling 'solve' function
## directly, first initialize cached matrix object by command like this:
##      MyCacheMatrix<-makeCacheMatrix(OriginalMatrix)
## and then get its inverse by calling 'cacheSolve' function like this:
##      cacheSolve(myCacheMatrix,...)
## where ellipsis stand for any additional parameters you would normally use
## with the 'solve()' function.
## Current values by cached matrix and its reverse can be called like this:
##      MyCacheMatrix$get()
##      MyCacheMatrix$getinverse()

## Here we assume that matrix supplied is always invertible and do not check
## for it to be square, according to assignement description, although in
## a real-life situation we would probably check for that first when
## initializing the object originally, and then when using 'set' function.

## This function creates a special "matrix" object that can cache its inverse.
## This 'object' is a list of four object manipulation function serving as its
## methods and its environment where both matrix and its reverse are stored.

makeCacheMatrix <- function(x = matrix()) {
## Before using this object, initialize it, either passing original matrix
## right away as a function parameter, or by using 'set' method like this:
##      MyCacheMatrix<-makeCacheMatrix()
##      MyCacheMatrix$set(OriginalMatrix)
  m <- NULL ## initialize the cached inverse
  set <- function(y) {
    x <<- y # let's pass new matrix to object's environment
    m <<- NULL # matrix's been changed, let's discard cached inverse
  }
  get <- function() x #what's current cached matrix value?
  setinverse <- function(solve) m <<- solve #solve is the inversed matrix
  # let's pass it it our 'object' enviroment to store
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) # return the list of our method functions
}


## The following function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if
## the inversehas already been calculated. If so, it gets the inverse from
## the cache and skips the computation. Otherwise, it calculates the inverse
## of the data and sets the value of the inverse in the cache via
## the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x',
        ## where 'x' can't be a matrix but should be special cachable matrix
        ## object instead.
  m <- x$getinverse()
  if(!is.null(m)) { #cached inverse is available, let's get it
    message("getting cached data")
    return(m) #mission accomplished, no need to run rest of the function
  }
  data <- x$get() 
  m <- solve(data, ...) #if we got here, cache was empty, let's calculate
  x$setinverse(m) #and remember the inverse in our special cacheMatrix object
  m #but return the inverse as well
}
