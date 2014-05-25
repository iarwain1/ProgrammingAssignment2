## Put comments here that give an overall description of what your
## functions do

## This function creates a special matrix which is really a list containing a
## function to (1) set the value of the matrix, (2) get the value of the matrix,
## (3) set the value of the inverse, and (4) get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function first checks to see if the inverse has been calculated.  If so,
## it gets the inverse from the cache and skips the computation. Otherwise, it 
## calculates the inverse of the data and sets the value of the inverse in the 
## cache via the solve function.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}