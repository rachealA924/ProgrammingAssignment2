## These two functions work together to create a special matrix object
## that can cache its inverse, avoiding repeated costly computations.
## If the inverse has already been computed and the matrix hasn't changed,
## the cached result is returned instead of recomputing.

## This function creates a special "matrix" object that can cache its inverse.
## It returns a list of 4 functions:
##   set        - store a new matrix
##   get        - retrieve the stored matrix
##   setinverse - store the computed inverse
##   getinverse - retrieve the cached inverse (NULL if not yet computed)

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y) {
        x   <<- y
        inv <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(inverse) inv <<- inverse
    
    getinverse <- function() inv
    
    list(set        = set,
         get        = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" object
## created by makeCacheMatrix. If the inverse has already been calculated
## and the matrix has not changed, it retrieves the inverse from the cache.
## Otherwise, it computes the inverse using solve() and stores it in the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    data <- x$get()
    inv  <- solve(data, ...)
    x$setinverse(inv)
    inv
}