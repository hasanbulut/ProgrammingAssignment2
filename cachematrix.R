## The following two functions compute the inverse of a matrix and cache the 
## result if the matrix has not been changed. makeCacheMatrix function provides
## set, get, setinv and getinv functions and cacheSolve function solves the 
## inverse of the matrix provided by makeCacheMatrix. The following lines are
## examples for how to use these two functions: 
## > source("CacheMatrix.R")
## > mtr <- c(4,3,3,2)
## > mtr <- matrix(mtr, nrow=2, ncol=2)
## > cachedMtrx <- makeCacheMatrix(mtr)
## > mtrxinv <- cacheSolve(cachedMtrx)
## > mtrxinv
## [,1] [,2]
## [1,]   -2    3
## [2,]    3   -4
## > mtrxinv <- cacheSolve(cachedMtrx)
## getting cached data
## > mtrxinv
## [,1] [,2]
## [1,]   -2    3
## [2,]    3   -4
## >  

## makeCacheMatrix function provides set, get, setinv and getinv functions
## It returns a list of functions defined 
## set function assigns matrix y to matrix x
## get function retrieve the matrix x
## setinv function sets the inverse of matrix x to invMtrx
## getinv function retrieves the value of invMtrx

makeCacheMatrix <- function(x = matrix()) {
        invMtrx <- NULL
        set <- function(y) {
                x <<- y
                invMtrx <<- NULL
        }
        get <- function() x
        setinv <- function(inv) invMtrx <<- inv
        getinv <- function() invMtrx
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve function solves the inverse of the matrix provided by 
## makeCacheMatrix. The inverse of the matix is stored in invMtrx. If invMtrx
## has already been computed the result is returned immediately. Otherwise it 
# is computed, stored and returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invMtrx <- x$getinv()
        if(!is.null(invMtrx)) {
                message("getting cached data")
                return(invMtrx)
        }
        data <- x$get()
        invMtrx <- solve(data, ...)
        x$setinv(invMtrx)
        invMtrx        
}
