## Note: Functions based on example functions: makeVector() and cachemean() at https://class.coursera.org/rprog-012
## Two functions below cache the inverse of a matrix, assuming matrix supplied is always invertible. 

## This function creates a list of functions to set the values of a matrix, get the values of the matrix,set
## the inverse of the matrix and get the inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {     # set the matrix with values 
                x <<- y
                i <<- NULL
        }
        get <- function()x       #return matrix
        setinv <- function(inv) i <<- inv #sets inverse of matrix supplied from cacheSolve
        getinv <- function() i            #returns the value of saved inverse
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}
## This function checks for a cached value for the inverse of matrix through a getinv() call.
## If the inverse exists i.e if not null, then that value is returned, otherwise the inverse is computed and cached
## through setinv(). It is assumed matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()                            #get the value of inverse in cache
        if(!is.null(i)){                           #if cache not empty return value
                message("getting cached inverse")
                return(i)
        }
        matrix <- x$get()
        i <- solve(matrix) #compute inverse of matrix
        x$setinv(i)
        i
}
