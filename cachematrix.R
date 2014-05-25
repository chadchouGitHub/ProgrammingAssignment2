## Put comments here that give an overall description of what your
## functions do

## This is function to make a list of empty function.

## define the makeCachMatrix function with x as argument.
## x is matrix
makeCacheMatrix <- function(x = matrix()) { 

        i <- NULL ## assign m is null, initinalize.
        set <- function(y) {
                x <<- y  ##assign x with y outside the environment that was defined.
                i <<- NULL ##assign i to null, and don't change outside this environment.
        }
        get <- function() x ## define get function by assign x vector to get.
        setinverse <- function(solve) i <<- solve ## define setinverse function by assign inverse martrix result to i vector.
        getinverse <- function() i ##define getinverse with i vector.
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse= getinverse) ## return list of functions to makeCachMatrix function.


}


## This function will find inverse matrix in catch before computing same matrix again.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        i <- x$getinverse() ##assign i vector by subseting x list's getinverse element.
        ## Use if to check i to find the inverser in the memory.
        if(!is.null(i)) {
                message("getting cached data")
                return(i) ## if i already in the data, just returen i to cachSolve function.
        }
        data <- x$get() ##assign data vector with get function from subseting x list.
        i <- solve(data, ...) ## caculate inverse matrix with solve function.
        x$setinverse(i) 
        i ## return i to cacheSolve function.
}
