## Assingment 2 - R Programming (Coursera) by JHU.
## Inverse of a matrix with cache feature. 
## This code uses as template the example given in the assignment. p
## ERA.

## makeCacheMatrix creates a list of functions that can set and retrieve
## the value of a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(y) inv <<- y
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## cacheSolve retrieves the value of the inverse of the matrix defined by 
## makeCacheMatrix if it has already been computed. Otherwise, it 
##computes and returns the inverse of the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
        
}
