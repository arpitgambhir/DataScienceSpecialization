## The functions return below calculate the inverse of a matrix. 
## The value of the inverse is saved in cache, once it has been calculated, as a special matrix.

## This function creates a special matrix for a given matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get, getinverse = getinverse, setinverse = setinverse)
}


## This function calculates the inverse of that special matrix and saves the value in cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
