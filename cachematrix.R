## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(mtx = matrix()) {
    inverse <- NULL
    ## set function sets the matrix
    set <- function(x) {
        mtx <<- x;
        inverse <<- NULL;
    }
    get <- function() return(mtx); ##returns the matrix
    setinv <- function(inv) inverse <<- inv;  ##sets the inverse
    getinv <- function() return(inverse); ##returns the inverse
    return(list(set = set, get = get, setinv = setinv, getinv = getinv))  ##returns the list
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinv()
    if(!is.null(inverse)) {
        message("Getting cached data...")
        return(inverse)
    }
    data <- x$get()  ##gets data
    invserse <- solve(data, ...)  
    x$setinv(inverse)  ##sets the inverse
    return(inverse)
}
