## These functions store a cache of the inverse matrix so that it does not have to be repeatedly calculated

## This function is the data type that the matrix needs to be created in in order to be cached.  It is a function of functions
##that set, get, set the inverse, and get the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## this function will check to see if there is a cached inverse matrix, if not, it will calculate the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'  
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached inverse matrix")
    return(i)
  }
  datamatrix <- x$get()
  i <- solve(datamatrix, ...)
  x$setinverse(i)
  i
}

