
## makeCacheMatric stores Matrix and its inverse
## cacheSolve returns cached copy of inverse matrix - if found 
## else it calculates inverse, returns it, and stores it for future reference

## creates matrix and returns requested info
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(m_inverse) inv <<- m_inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}


## read cached inverse. If null, calculate it and store for future reference
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
