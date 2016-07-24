## A pair of functions that create a special object that store a matrix and cache the inverse of the matrix


## This function creates a special matrix object and stores the cached values of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
  xinverse <- NULL
  set <- function(y) {
    x <<- y
    xinverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) xinverse <<- solve
  getinverse <- function() xinverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function looks to see if the matrix inverse has been calculated, if yes, then
## it gets it from the cache, if no, then it computes it and the value is cached.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  xinverse <- x$getinverse()
  if(!is.null(xinverse)) {
    message("getting cached data")
    return(xinverse)
  }
  data <- x$get()
  xinverse <- solve(data, ...)
  x$setinverse(xinverse)
  xinverse
}
