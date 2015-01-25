## This function stores some sort of matrix item so it can be cached
## The first functionstores the cache item

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    temp <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inverse <<- inverse
    getinverse <- function() inverse
    list(set=temp, get=get, setinverse=setinverse, getinverse=getinverse)
}

## I hope this second function accespts what the first function does

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$setinverse(inverse)
    inverse
}
