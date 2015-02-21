## Matrix objects capable of storing their own inverse.  
##
## Example usage:
##
##    > ## Construct a random 3 x 3 matrix
##    > X = matrix( runif(9), nrow=3, ncol=3 )
##    > ## Construct the caching version
##    > cacheX = makeCacheMatrix(X)
##    > ## Calculate the inverse (this may return an error if X is singular or difficult to invert)
##    > cacheSolve(cacheX)
##    > ## Calculate the inverse again.  This time we will see a message indicating
##    > ## that we are getting the inverse from cache. 
##    > cacheSolve(cacheX)
## 

## Constructs an R object containing a matrix and capable of caching the inverse of the 
## matrix.  
## Arguments:
##   x - an invertible square matrix

makeCacheMatrix <- function(x = matrix()) {
  xInv <- NULL
  set <- function(y) {
    x <<- y
    ## Since we changed x our cached inverse is no longer correct, null it out
    xInv <<- NULL 
  }
  get <- function() x
  setInverse <- function(xInverse) xInv <<- xInverse
  ## This function returns the cached inverse or NULL if we have no
  ## valid inverse in cache
  getInverse <- function() xInv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Returns the inverse of the given cachematrix.  
## The first invocation will attempt to calculate the inverse of the matrix
## Subsequent invocations will return the answer from cache unless
## the matrix is changed.
## Arguments:
##   x - a cachematrix object, the output of a call to makeCacheMatrix

cacheSolve <- function(x, ...) {
  xInv <- x$getInverse()
  if(!is.null(xInv)) {
    message("getting cached data")
    return(xInv)
  }
  data <- x$get()
  xInv <- solve(data, ...)
  x$setInverse(xInv)
  xInv
}
