## The two functions here may be used to create a matrix whose inverse
## is cached the first time it is computed, to save computation time
## when the inverse is needed in the future.

## Creates a wrapped matrix object that can contain a cache of its
## inverse, and returns a list of operations for getting and setting
## the components of the object.

makeCacheMatrix <- function(x = matrix()) {
        cachedInverse <- NULL
	set <- function(newMatrixValue) {
	        x <<- newMatrixValue
		cachedInverse <<- NULL
	}
	get <- function() x
        setInverse <- function(newInverse) cachedInverse <<- newInverse
	getInverse <- function() cachedInverse
	list(set = set,
	     get = get, 
	     setInverse = setInverse,
	     getInverse = getInverse)
}


## Returns the inverse of one of the wrapped matrix objects created by
## the makeCacheMatrix function above, either by computing it directly
## and expensively and then storing the result in the cache, or else
## by retrieving it from the cache if it has already been computed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        cachedInverse <- x$getInverse()
	if (!is.null(cachedInverse)) {
                message("getting cached inverse")
		return(cachedInverse)
	}
	matrixValue <- x$get()
	message("solving for a new matrix inverse.")
	cachedInverse <- solve(matrixValue, ...)
	x$setInverse(cachedInverse)
	cachedInverse
}
