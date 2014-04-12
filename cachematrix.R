## Main goal of the functions is to cache the inverse of the matrix
## and avoid recalcuations. makeCacheMatrix creates a vector with
## necessary functions (explained below) and cacheSolve computes
## (if necessary) the inverse of the matrix.

## makeCacheMatrix creates a vector of functions to: set value of
## matrix, get value of matrix, set inverse and get inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve first checks to see if the inverse of the matrix
## is computed. If it is then it returns the inverse. If not, then
## it computes the inverse and stores it in cache using setInverse.

cacheSolve <- function(x, ...) {

    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat)
    x$setInverse(inv)
    inv
}
