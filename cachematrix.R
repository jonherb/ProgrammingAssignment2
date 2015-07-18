## These functions allow for obtaining the inverse of a matrix object,
## with the computation only being performed if there is no previously
## cached result of an inverse computation for the matrix object. Thus,
## for the matrix stored in makeCacheMatrix, cacheSolve will return the
## inverse. If this computation on the same makeCacheMatrix object has
## previously been executed, cacheSolve will not do the computation again,
## but instead retrieve the result stored in the makeCacheMatrix object.

## makeCacheMatrix takes a matrix as its argument and returns a list of 
## functions defined in its scope. These are get, which returns the input
## matrix, set, for setting the input matrix to a new value, getinv, for
## retrieving the cached inverse matrix (or NULL if this has not been
## computed yet), and setinv which sets its argument the inverse matrix cache.
makeCacheMatrix <- function(x = matrix())  {
      i <- NULL
      set <- function(y)  {
        x <<- y
        i <<- NULL
      }  
      get <- function() x
      setinv <- function(inv) i <<- inv
      getinv <- function() i
      list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## CacheSolve takes a makeCacheMatrix object (x) as its argumnent, and returns 
## for the matrix stored there, (retrieved by x$get), the inverse. First it checks
## if the cache for the inverse matrix (i) has a non-null value. If so, it returns
## this cached inverse matrix (along with a warning message that this is cached data).
## Otherwise, on the matrix stored in its argument makeCacheMatrix object argument,
## it does the solve computation, then retrieves the setinv function to set the 
## matrix inverse solution to i in the makeCacheMatrix scope, thereby caching the 
## solution, so that if cacheSolve is done again on the same makeCacheMatrix object,
## the cached result is returned, rather than the computation being executed again.
cacheSolve <- function(x, ...) {
      i <- x$getinv()
      if(!is.null(i))  {
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinv(i)
      i
}