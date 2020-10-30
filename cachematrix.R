## These two functions cache the inverse of a matrix rather than compute it
## repeatedly.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        matinv <- NULL
        set <- function(y) {
                x <<- y
                matinv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) matinv <<- inverse
        getinverse <- function() matinv
        list(set = set, get = get, setinverse = setinverse, 
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matinv <- x$getinverse()
        if(!is.null(matinv)) {
                message("getting cached data")
                return(matinv)
        }
        data <- x$get()
        matinv <- solve(data, ...)
        x$setinverse(matinv)
        matinv
}
