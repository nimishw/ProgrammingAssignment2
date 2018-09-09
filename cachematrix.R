##Caching the inverse of a matrix
##There are two functions, makeCacheMatrix, and cacheSolve
##I have tried to adapt the vector example as much as possible.

##This function makes a list of functions:
##set: This sets the values in the matrix whose inverse is to be calculated
##get: This gets the values in the above matrix
##setinverse: This sets the values in the inverse of the matrix
##getinverseThis gets the values in the inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}

##This function calculates the inverse of the matrix that we set using the set function above.
##If the inverse has already been calculated and the matrix is not changed, the inverse is retrieved from the cache.

cacheSolve <- function(x, ...) {
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
