## Matrix inversion is usually a costly computation 
## and there may be some benefit to caching the inverse 
## of a matrix rather than compute it repeatedly. 
## Following functions provides caching approach to 
## maximize performance. 

## Creates a list of functions that can 
## cache the inverse of a matrix.
## List function to: 
##  1. set the value of the Matrix
##  2. get the value of the Matrix 
##  3. set the Inverse 
##  4. get the Inverse
makeCacheMatrix <- function(x = matrix()) {
    
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<-inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## Computes the inverse of the matrix returned
## by makeCacheMatrix(), unless the inverse has
## already been calculated, in which case
## it retrieves it from the cache.

cacheSolve <- function(x, ...) {
  
  m <- x$getInverse()
  if ( ! is.null(m)) {
    print("getting cached data")
    return(m)
  }
  m <- solve(x$get())
  x$setInverse(m)
  m  
}
