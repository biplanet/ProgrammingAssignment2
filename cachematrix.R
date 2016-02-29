## Cashing matrix inverse

## The first function, "makeCacheMatrix" creates a special "matrix" and creates
## set & get functions for the matrix for cashing purpose


makeCacheMatrix <- function(x = matrix()) {
    matrixInverse <- NULL
    set <- function(y) {
        x <<- y
        matrixInverse <<- NULL
    }
    
    
    get <- function() x
    setcashMatrix <- function(mt) {
        matrixInverse <<- mt 
        
    }
    
    getcashMatrix <- function() matrixInverse
    list(set = set, get = get,
         setcashMatrix = setcashMatrix,
         getcashMatrix = getcashMatrix)
    
}


#The following function calculates the inverse of the special "matrix"
#created with the above function. However, it first checks to see if the
#inverse has already been calculated. If so, it gets the inverse from the
#cache and skips the computation. Otherwise, it calculates the inverse of
#the matrix and sets the value of the inverse in the cache via the "setcashMatrix"
#function.

cacheSolve <- function(x, ...) {
    
    cashedMatrixInverse <- x$getcashMatrix()
    if (!is.null(cashedMatrixInverse)) {
        message("getting cached data")
        return(cashedMatrixInverse)
    }
    getMatrix <- x$get()
    cashedMatrixInverse <- solve(getMatrix, ...)
    x$setcashMatrix(cashedMatrixInverse)
    cashedMatrixInverse
}
