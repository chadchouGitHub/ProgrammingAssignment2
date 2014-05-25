## Put comments here that give an overall description of what your
## functions do

## This is function to make a list of empty function.

## define the makeCachMatrix function with x as argument.
## x is matrix
makeCacheMatrix <- function(x = matrix()) { 

        i <- NULL ## assign m is null, initinalize.
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse= getinverse)


}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

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
