## These two functions provide a way to hold
## a matrix and calculate and casche the inverse
## of the matrix

## This function holds the matrix and creates the
## setter and getter methods for the matrix and
## the cached inverse. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function (y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse = function(inverse) m <<- inverse
  getinverse = function() m
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}




##This function checks the cache and returns the cached
##inverse. If the inverse has not been cached it
##calculates the inverse and stores it in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
