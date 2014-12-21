## Creating a "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) 
{
    inv <- NULL
    set <- function(m)
    {
        x <<- m
        inv <<- NULL
    }
    get <- function() x
    
    setinv <- function(inverted) inv <<- inverted
    getinv <- function() inv
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Calculating the inverse of the "matrix" or getting it from cache

cacheSolve <- function(x, ...) 
{
    inv <- x$getinv()
    if (! is.null(inv))
    {
        message("getting cached data")
        return(inv)
    }
    
    message("calculating inverse of a matrix")
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}

