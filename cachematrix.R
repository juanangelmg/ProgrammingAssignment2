##
## This function creates a special "matrix" object that can cache its inverse
##
makeCacheMatrix <- function(x = matrix()) {
    matrixInverse <- NULL 
    set <- function(y) {
        x <<- y
        matrixInverse <<- NULL
    }
    
    get <- function() x
    setMatrixInverse <- function(inv) matrixInverse <<- inv
    getMatrixInverse <- function() matrixInverse
    list(set = set, get = get, 
         setMatrixInverse = setMatrixInverse, 
         getMatrixInverse = getMatrixInverse)
}



## 
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve retrieve the inverse from the cache.
##

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getMatrixInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    matrix <- x$get()
    m <- solve(matrix)
    x$setMatrixInverse(m)
    m
}