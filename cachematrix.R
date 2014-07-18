## The listed functions shall be used for calculating an inverse of matrix
## or if once calculated - for retrieving of prevously calculated inverse.
## Assumption is that the matrix is invertible. Automatic retrieve will
## occur only if the originally parsed matrix has not been changed.

## General process description:
## 1) Parse a square invertible matrix through the "makeCacheMatrix"
## 2) Parse the outcome of the function through the second function
## "cacheSolve". This would either prompt calculation of the inverse or
## load already stored result from a previuos run.


## makeCacheMatrix function will store any user defined matrix. 
## It's purpose is to store the matrix and potentially its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve function will either calculate the inverse of a submitted matrix 
## in the previuos function or will retrieve its already stored inverse from
## previuos calls.

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
