## The following functions are used to solve for solving for matrix inverse
## and caching the result.

## brief:   Stores a matrix and its inverse which can then be retrieved
## in:      initial matrix to be constructed
## out:     list of functions associated with setting/getting the cache
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

## brief:   Solves for a matrix inverse given a cached matrix and stores the
##          inv in the cache if it has changed
## in:      Takes in a list of functions associated with a cached matrix,
##          see: makeCacheMatrix - out
## out:     inverse of the matrix
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv))
    {
        message("getting cached data")
        return(inv)
    }
    xmat <- x$get()
    inv <- solve(xmat, ...)
    x$setinverse(inv)
    
    ## Return a matrix that is the inverse of 'x'
    inv
}
