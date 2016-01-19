## Assignment 2 - Caching the Inverse of a Matrix

## function to create a 'special' matrix that caches the inverse 
## of a matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL ##clear previous inverse 
        }
        get <- function() x
        setinverse <- function(inv) i <<- inv
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## function that returns the inverse of a matrix
## if the matrix has not changed and the inverse was
## previously calculated it returns the cached value
## otherwise it calculates it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        
        ## check if inverse is already calculated
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i        
}
