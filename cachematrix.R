## This example illustates how we can take advantage of scoping rules of the R language and cache inverse of a Matrix
## makeCacheMatrixPut creates a special Matrix from a normal Matrix and cachesolve returns inverse of the matrics 
## from cache if available and calculates inverse if it is not available in cache

## This function creates a list having the functions Set, get, getinverse and setinverse from a Matrix, assuming 
## the Matrix is inversible

makeCacheMatrix <- function(x = matrix()) 
  {
    inverse <- NULL
  
    set <- function(y) 
      {
        x <<- y
        inverse <<- NULL
      }
  
    get <- function() x
  
    setinverse <- function(Minverse) inverse <<- Minverse
  
    getinverse <- function() inverse
  
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }


## This functions return inverse of a Matrix. It calculates the inverse if inverse Matrix is not avaiable in cache
## else gets the inverse of the Matrix from cache
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) 
  {
  
    inverse <- x$getinverse()
  
    if(!is.null(inverse)) 
      {
        message("getting inverse from cache")
        return(inverse)
      }
  
    xMatrix <- x$get()
  
    inverse <- solve(xMatrix, ...)
  
    x$setinverse(inverse)
  
    inverse
  
  }
