## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix provides getter and setter methods to
## get and set the original matrix
## get and set the inverted matrix (cached)
makeCacheMatrix <- function(x = matrix()) {
    invM <- NULL
    set <- function(y) {
        x <<- y
        invM <<- NULL
    }
    get <- function() x
    setinvM <- function(inverse) invM <<- inverse
    getinvM <- function() invM
    list(set = set, get = get,
         setinvM = setinvM,
         getinvM = getinvM)
    
}


## Write a short comment describing this function
## provides an inverted matrix efficinetly
## 1) if matrix was cached - returns the cached value 
## 2) if not cached, computes the inverted result and caches it
cacheSolve <- function(x, ...) {
    invM <- x$getinvM()
    if(!is.null(invM)) {
        message("getting cached data")
        return(invM)
    }
    data <- x$get()
    invM <- solve(data, ...)
    x$setinvM(invM)
    invM
}
