## This function creates a special "matrix" object that can cache its inverse.

## Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation and caching it can save time rather than computing it repeatedly 

## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Return the inverse of an cacheMatrix object

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
