
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
  
  ## Initialize variable to store solution as NULL to indicate that 
  ## matrix has not yet been solved
  m <- NULL
  
  ## This function will reset the value of the initial matrix, and 
  ## reset m to NULL to indicate that the inverse needs to be recalculated
    set <- function(y) {
    
    x <<- y
    
    ## Reset solution variable to NULL to indicate  matrix needs to 
    ## be resolved
        m <<- NULL
  }
  
  ## Retrieve value of initial matrix
  get <- function() x
  
  ## Solve the matrix and store the inverse
  setsolve <- function(solve) m <<- solve
  
  ## Retrive the inverse of the matrix
  getsolve <- function() m
  
  ## Return the list of functions stored in this vector
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
  
}


## Check to see if the matrix has been solved. If it has, return the value
## from the cache. If it has not, calculate the solution and store it in 
## the cache. 

cacheSolve <- function(x, ...) {
  
  ## Retrive the inverse of the matrix - if one exists, m will have a value. If
  ## not, m will return NULL
  m <- x$getsolve()
  
  ## If m has a value, return the value, which is the inverse of the initial matrix
  if(!is.null(m)) {
    message("getting cached data")
    
    ## Return the value of the solution, and exit the function
    return(m)
  }
  
  ## If a solution is not stored, we need to calculate and store one.
  
  ## Calculate the inverse
  m <- solve(x$get(), ...)
  
  ## Cache the inverse
  x$setsolve(m)
}

