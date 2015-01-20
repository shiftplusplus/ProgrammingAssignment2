## These functions provide a framework for efficient matrix 
#inversion. makeCacheMatrix(x=matrix()) makes a specialzed version 
#of the matrix that can store the inversion once requested, and 
#cacheSolve(x,...) returns the inverted matrix.

## In accordance with the instructions to this assignment,
# matrices are assumed to be invertible.

## makeCacheMatrix takes an optional matrix and converts it to our
# specialized version. Returns a list of functions.
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) inverse <<- solve
    getsolve <- function() inverse
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## cacheSolve(x,...) obtains the inversion of the "matrix"

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getsolve()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setsolve(inverse)
    inverse
}
