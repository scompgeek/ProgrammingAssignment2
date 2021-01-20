## This code checks to see if a matrix invert has already been computed
## If it has, it returns the cached result.
## If it hasn't, it performs the computation and caches it.

## This function creates a matrix that can cache the inverse

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinvert <- function(solve) s <<- solve
  getinvert <- function() s
  list(set = set, get = get,
       setinvert = setinvert,
       getinvert = getinvert)
}


## This function computes the inverse if it has not already been cached

cacheSolve <- function(x, ...) {
  s <- x$getinvert()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinvert(s)
  s
  ## Return a matrix that is the inverse of 'x'
}
