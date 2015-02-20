
## SUMMARY
## ---------------------------------------------------------------
## The following functions users to create, store, and retrieve
## the inverse of a given invertible matrix. Once the inverse of
## the given matrix has been solved and cached, the function will
## return the value from the cache, versus re-running the function
## to solve the matrix. If the matrix has changed, it will be
## resolved and the new value will be stored in the cache. 

## Create a list comprised of functions to set and retrieve the value
## of the matrix, and to set and retrieve the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
  
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
}

