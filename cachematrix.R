## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

## Creating an object 'matrix'...
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
## Enable the object to cache the matrix's inverse...
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## Write a short comment describing this function

## The computation for the inverse matrix...
cacheSolve <- function(x, ...) {
        
        ## When the matrix is not modified, and the inverse matrix exists,
        ## the inv will be retrieved.
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## Or, return a matrix that is the inverse of 'x'...
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
