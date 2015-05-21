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

## Before using this object, initialize it, either passing original matrix
## right away as a function parameter, or by using 'set' method like this:
##      MyCacheMatrix<-makeCacheMatrix()
##      MyCacheMatrix$set(OriginalMatrix)

makeCacheMatrix <- function(x = matrix()) {
  # initialize the cached inverse
  m <- NULL
  
  # outside of this exercise it would make sense to check if we received
  # an object of the matrix class indeed, and cry out if we haven't
  
  # 1. this is a method to initialize or update the source matrix
  set <- function(y) {
    # let's pass new matrix to object's environment
    x <<- y 
    # matrix's been changed, let's discard cached inverse
    m <<- NULL 
  }
  
  # 2. this is a method to return value of the stored matrix
  get <- function() x 
  
  # 3. this is a method to store calculated inverse
  # it sets value of cached inverse in higher level enviroment
  setinverse <- function(inversed) m <<- inversed
  
  # 4. this is a method to retrieve inverse once calculated
  # it's a duty of a calling code to check if it hasn't been yet,
  # and returned value is NULL
  getinverse <- function() m
  
  # this is what our constructor function will return:
  # a list of four named method functions that will access
  # original and inversed matrices stored in this environment 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) 
}


## The following function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if
## the inverse has already been calculated. If so, it gets the inverse from
## the cache and skips the computation. Otherwise, it calculates the inverse
## of the data and sets the value of the inverse in the cache via
## the setinverse function.

## The argument of this function is supposed to be not a value of a class matrix,
## but rather a special object created by the constructor function above.
## It will return an object of matrix class though.

cacheSolve <- function(x, ...) {
  
  # let's check if we already have inverse stored
  inversed <- x$getinverse()
  
  # if cached inverse is available, let's return it right away
  if(!is.null(inversed)) { 
    message("getting cached data")
    # mission accomplished, no need to run rest of the function
    return(inversed) 
  }
  
  # if we are still here, then stored inverse was NA, let's calculate it
  # first, retrieve the original matrix
  original <- x$get() 
  # then, run solve() function, passing all additional parameters as arguments
  # this is what was given in our example, although wouldn't it affect results if
  # parameters were to change, and discredit the whole cached model?
  inversed <- solve(original, ...)
  # now let's store it in our cacheMatrix object
  x$setinverse(inversed)
  # and return the inverse as well
  inversed 
}
