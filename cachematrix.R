## Caching the inverse of a matrix

## Create a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    set_inv <- function(solve) inv <<- solve
    get_inv <- function() inv
    list(set = set, get = get,
         set_inv = set_inv,
         get_inv = get_inv)
}


## Compute the inverse of the special matrix returned by makeCacheMatrix if
## it hasn't been calculated yet, or retrieve the cached inverse otherwise

cacheSolve <- function(x, ...) {

    inv <- x$get_inv()
    if(!is.null(inv)) {
        message("Getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$set_inv(inv)
    inv
}
