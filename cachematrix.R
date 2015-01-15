## Two functions to store a matrix and cache its inverse.

## Function 1 creates a list of functions to set the matrix, get the matrix, 
## set the inverse, and get the inverse. 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {  ## The set function can be called to change the matrix values.
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)        
}


## Function 2 returns the cached matrix inverse if previously calculated, or 
## calculates it if not.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}