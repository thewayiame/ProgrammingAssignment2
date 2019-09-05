## Caching the Inverse Matrix of a Matrix
## functions do

## makeCacheMatrix creates a special vector containing a function to
## set the value of a matrix
## get the value of a matrix
## set the value of the inverse matrix
## get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve creates a special vector created with the above function
## if the inverse matrix has already been calculated
##      gets the inverse matrix from the cache and skips the computation 
##      of inverse matrix
## Otherwise, calculates the inverse matrix of the data and sets the value 
##      of the inverse matrix in the cache via the setSolve function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached inverse matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
