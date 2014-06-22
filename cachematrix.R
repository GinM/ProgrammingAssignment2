## The following 2 functions are for Coursera R Programming Course:Assignment 2

## file name is 'cachematrix.R'

## The following function, 'makeCacheMatrix',  creates a special
## "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    
    I <- NULL
    
    Set <- function(y) {
        x <<- y
        I <<- NULL
    }
    
    Get <- function() x
    
    SetInv <- function(inv) I <<- inv
    
    GetInv <- function() I
    
    list(set = Set, get = Get, setInv = SetInv, getInv = GetInv)
}


## The following function, 'cacheSolve' computes the inverse of the
## special "matrix" returned by makeCacheMatrix above. If the inverse has
## already been calculated (and the matrix has not changed), then the cacheSolve
## function should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    I <- x$getInv()
    
    if(!is.null(I)) {
        message("getting cached data")
        return(I)
    }
    
    data <- x$get()
    
    I <- solve(data, ...)
    
    x$setInv(I)
    
    I
}

