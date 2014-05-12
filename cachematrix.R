## The functions in this file find the inverse of a matrix. Since this is
## an expensive operation, they provide a caching mechanism, so that the
## inverse is only computed when necessary

## This function creates a matrix-like structure that can cache its inverse.
## The structure has methods set(x) and get() that provide access to the
## underlying matrix. It also has the method getinverse(), which returns the
## multiplicative inverse of the matrix. If getinverse() returns null, it is
## the caller's responsibility to set the inverse first using setinverse().
## See cacheSolve() for an example of proper usage

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
            x <<- y
            inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function returns the inverse of a matrix built with makeCacheMatrix().
## It first looks to see if the inverse has already been computed. If not, it
## computes the inverse and stores the value in a cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
