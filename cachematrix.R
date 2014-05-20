# Description
# 
# This code includes two functions makeCacheMatrix and cacheSolve:
#
# 1. makeCacheMatrix creates a special matrix object that can cache its inverse.
# 2. cacheSolve returns a matrix that is the inverse of 'x', which would be from cache if it 
#    has been computed before and 'x' hasn't changed.

#### makeCacheMatrix ####

# This function creates a special matrix object that can cache its inverse. This special object 
# provides the following functions for setting and retrieving the matrix object and its inverse:
#     set(x) - store matrix x in object
#     get(x) - retrieve matrix x stored in the object
#     setsolved(solved) - store the inversed matrix solved in object
#     getsolved() - retrieve the inversed matrix solved cached in the object

makeCacheMatrix <- function(x = matrix()) {
        # define the getter/setting functions
        set <- function(y) {
                # ensure input is a square matrix
                if (!is.matrix(y) || dim(y)[1] != dim(y)[2] ) {
                        stop("x must be a square matrix")
                }                
                x <<- y
                solved <<- NULL
        }
        get <- function() x
        setsolved <- function(solved) solved <<- solved
        getsolved <- function() solved
        
        # call setter function to initialze x
        set(x)
        
        list(set = set, get = get,
             setsolved = setsolved,
             getsolved = getsolved)
}

#### cacheSolve ####

# This function checks if the inverse has already been calculated (and the matrix has not changed).
# If yes then it will retrieve the inverse from the cache.
# Otherwise, it will compute the inverse, store it in cache then return it.

cacheSolve <- function(x, ...) {        
        solved <- x$getsolved()
        if(is.null(solved)) {
                # nothing in cache, let's get 'x' and compute the inverse
                data <- x$get()
                solved <- solve(data, ...)
                x$setsolved(solved)
        }        
        else
                message("getting cached data")
        
        # Return inverse
        solved
}