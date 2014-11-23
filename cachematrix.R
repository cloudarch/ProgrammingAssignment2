## Caching the Inverse of a Matrix 
## Two helper functions to cache inverse of matrix to save repeated long running computations 
## for the same result

## makeCacheMatrix : 
## caches given matrix and its inverse into global space and
## returns a list of constructors to get or set global variables above


makeCacheMatrix <- function(x = matrix()) {
  
  s <- NULL
  
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  
  get <- function() x
  
  setsolve <- function(solve) s <<- solve
  
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## cacheSolve : 
## Return inverse of given CacheMatrix if in cache else first calculate, cache and finally return

cacheSolve <- function(x = makeCacheMatrix, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}