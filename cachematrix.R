## This is an R function able to cache potentially time-consuming computations

## This function creates a special "matrix" object that can cache its inverse
## it sets the value of the matrix; gets the value of the matrix;
## sets the inverse of the matrix and gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  invs <- NULL
  set <- function(y) {
    x <<- y
    invs <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invs <<- inverse
  getinverse <- function() invs
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


##  This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invs <- x$getinverse
  if(!is.null(invs)) {
    message("getting cached data")
    return(invs)
  }
  data <- x$get()
  invs <- inverse(data, ...)
  x$setinverse(invs)
  invs
}
