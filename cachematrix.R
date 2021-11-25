## first, setting the function then getting the function then setting/getting the function
## I called invrs for inverse and mt for getting the function.

makeCacheMatrix <- function(x = matrix()) {
        invrs <- NULL
        set <- function(y) {
               x <<- y
               invrs <<- NULL
        }
        get <- function() {x}
        setinverse <- function(inverse) {invrs <<- inverse}
        getinverse <- function() {invrs}
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
        invrs <- x$getinverse()
        if(!is.null(invrs)){
                message("getting cached data")
                return(invrs)
        }
        mt <- x$get()
        invrs <- solve(mt, ...)
        x$setinverse(invrs)
        invrs
}
