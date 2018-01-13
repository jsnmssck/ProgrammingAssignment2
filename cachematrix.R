## Functions to cache the inverse of a matrix. If the inverse
## has already been calculated, it will be returned from the cache
## instead of being recalculated.

## Usage: 
## > mat <- matrix(c(1,2,3,4), nrow = 2, ncol = 2) # create a square matrix
## > cm <- makeCacheMatrix(mat)                    # create caching matrix
## > cacheSolve(cm)                                # call cacheSolve
## > cacheSolve(cm)                                # another call returns cached
                                                   # value

## Creates a special "matrix" object with functions to cache its inverse
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


## Computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cacheSolve retrieves the inverse 
## from the cache.
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if (!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
