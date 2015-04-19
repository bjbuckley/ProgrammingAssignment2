## This is a pair of functions that initially calculates the inverse
## of a matrix and cache's its result, the second function checks to see
## if the result has already been calculated before calculating again.
## If the result has already been calculated, it calls the result from
## the cache.

## This function creates a special "matrix" that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated (and
## the matrix has not changed) the the cache solve should retrieve the
## inverse from the cache.

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
