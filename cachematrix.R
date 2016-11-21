## Calculating the inversion of the matrix is a costly computation.  The makeCacheMatrix and cacheSolve 
## functions together cache the inverse of the matrix to avoid computing the inverse repeatedly

## The makeCacheMatrix function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## This function computes the inverse of the special matrix returned by makeCacheMatrix.  
## If the inverse is already calculated, this function will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	  i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data,...)
        x$setinv(i)
        i
}
