## Functions to cache the inverse of a matrix to avoid 
## costly repeated computations


## "makeCacheMatrix" creates a "special matrix object"
## returns a list of functions which:
##      1. sets the matrix
##      2. gets the matrix
##      3. sets the inverse 
##      4. gets the inverse

makeCacheMatrix <- function(x = matrix()) {
  i = NULL
  
  set <- function(y) {
    ## "<--" assigns a value to an object in an environment 
    ## that is different from the current environment
    x <<- y 
    i <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) i <<- inverse
  
  getinverse <- function() i
  
  list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## 'cacheSolve' returns the inverse of the "matrix" returned by
## 'makeCacheMatrix'

cacheSolve <- function(x, ...) {
  
  i <- x$getinverse()
  
  ## If the inverse has already been calculated, it retrieves
  ## the inverse from the cache.
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  
  ## Otherwise, computes the inverse itself
  data <- x$get()
  
  i <- solve(data,...)
  
  x$setinverse(i)
  
  return(i)
}

