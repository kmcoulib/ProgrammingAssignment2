## This program caches the inverse of a matrix, uses the cached result
## for subsequent calculations if the input hasn't change and the 
## inverse doesn't need to be recomputed.
## makeCacheMatrix: creates a special list with a set of functions
##                  and caches the inverted matrix
## cacheSolve: Checks the existence of the inverted matrix, reads and
##             outputs the cached values or computes the inverted 
##             if it hasn't been cached by makeCacheMatrix
#####################################################################                   


## Creates a list of functions and inverts and caches a matrix
makeCacheMatrix <- function(x = matrix()) {
    minv <- NULL
    set <- function(y) {
        x <<- y
        minv <<- NULL
    }
    getmat <- function() x
    setminv <- function(solve) minv <<- solve
    getminv <- function() minv
    list(set = set, getmat = getmat,
         setminv = setminv,
         getminv = getminv)
}


## Checks if a cached inverted matrix was computed and returns, computes the
## inverse otherwise
cacheSolve <- function(x, ...) {
    minv <- x$getminv()
    if(!is.null(minv)) {
        message("getting cached data")
        return(minv)
    }
    data <- x$getmat()
    minv <- solve(data, ...)
    x$setminv(minv)
    minv
}