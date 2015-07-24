## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Create object containing a list of functions to
#  set and get the matrix. It also implements
#  functions to set and get its inverse, which
#  will be used as the cache by the cacheSolve function below
makeCacheMatrix <- function(x = matrix()) {
    invMatrix <- NULL
    #Set matrix and reset cache
    set <- function(m) {        
        x <<- m
        invMatrix <- NULL
    }
    #Get matrix
    get <- function() x
    #Get the cached inverse matrix
    getInverse <- function() invMatrix
    #Set the cached inverse matrix
    setInverse <- function(inv) {
        invMatrix <<- inv
    }

    #Return the list of implemented functions
    list(set=set, get=get, getInverse=getInverse, setInverse=setInverse)
}


# Return the inverse of a matrix, checking if it's already cached first
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if (!is.null(inverse)) {
        message("Returning inverse matrix cached.")
        return(inverse)
    }
    #Otherwise we need to compute the inverse and store
    # the result in the cache
    originalMatrix <- x$get()
    inverse <- solve(originalMatrix)
    x$setInverse(inverse)
    #Now return the inverse
    inverse
}
