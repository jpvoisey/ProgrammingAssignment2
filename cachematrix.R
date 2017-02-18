## makeCacheMatrix returns an object that allows a matrix and it's inverse to be cached
## cacheSolve checks to see if inverse has been cached, calculates if not, then returns inverse

## Create list to set & get matrix and to set & get it's inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(invm) inv <<- invm
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv, getinv = getinv)
}


## Check if inverse has been cached, calculate if necessary, then return it
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinv(inv)
    inv
}




