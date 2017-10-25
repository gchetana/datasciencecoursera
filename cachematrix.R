## Caching the Inverse of a Matrix
## Below pair of functions caches the inverse of a matrix

## makeCacheMatrix function creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
          inverse<-NULL
          set <- function(y) {
                x <<- y
                inverse <<- NULL
          }
          get <- function() x
          setinverse <- function(setinverse) inverse <<- setinverse
          getinverse <- function() inverse
          list(set = set,
               get = get,
               setinverse = setinverse,
               getinverse = getinverse)
}


# cacheSolve function computes inverse of matrix created by makeCacheMatrix, or inverse from cache if it has already been computed.

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)){
            message("Getting cached data")
          return(inverse)
        }
        matrx <- x$get()
        inverse <- solve(matrx, ...)
        x$setinverse(inverse)
        inverse
}
