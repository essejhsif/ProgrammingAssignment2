## A pair of functions that cache the inverse of a matrix. 
## For example:
## m<-makeCacheMatrix()   - will first set up a matrix
## m$set(matrix(1:4,2,2)) - will set the matrix with data 
## cacheSolve(m)          - will return the inverse of the matrix
## Calling cacheSolve(m) again will return the cached inverse of the matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

    m <- NULL

    ## Set function for the matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## Get function for the matrix
    get <- function() x

    ## Set function for the inverse matrix
    setinv <- function(inverse) m <<- inverse
    
    ## Get function for the inverse matrix
    getinv <- function() m

    ## Return the matrix (with get/set functions)
    list(set = set, get = get, 
         setinv = setinv, 
         getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## If the inverse has already been calculated (and the matrix has not changed), then 
## the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getinv()

    ## If the inverse has already been calculated, return cached data
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }

    ## Compute the inverse of the matrix
    data <- x$get()
    m <- solve(data, ...)

    ## Now cache the inverse of the matrix
    x$setinv(m)

    ## Return a matrix that is the inverse of 'x'
    m
}
