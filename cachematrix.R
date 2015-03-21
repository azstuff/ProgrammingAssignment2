## Author: Adam Ziskind
## Date: 2015-03-20
## Description: The set of functions contained within provide the ability
## to cache the inverse of a matrix so the inverse of a given matrix
## only needs to be calculated once, but may be retrieved many times.

## === Test Code ===
## > x <- matrix(c(4,3,3,2), 2, 2)
## > y <- makeCacheMatrix(x)
## > cacheSolve(y)   ## call multiple times to retrieve from cache
##


## Function: makeCacheMatrix
## Param 1: x - original matrix
## Description: An instance of this function serves as a cache for
## both a single matrix and the inverse of the given matrix. Contained
## functions provide the ability to get and set both the original matrix
## and its inverse. Note that the inverse matrix is calculated outside of
## this function.
makeCacheMatrix <- function(x = matrix()) {
	## Define inverse matrix storage
	inv <- NULL
	
	## Define setters and getters
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	
	## Return list of setters and getters
	list(set = set,
		get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## Function: cacheSolve
## Param 1: x - Instance of makeCacheMatrix function
## Description: This function takes the passed in makeCacheMatrix, x, gets
## the matrix contained within, calculates the inverse matrix, and stores
## the inverse matrix back in x. If the inverse matrix has already been
## calculated, then the inverse matrix is returned directly from x as
## no recalc is required.
cacheSolve <- function(x, ...) {
	## Get the inverse of X from the provided cache
	inv <- x$getinverse()
	
	if (!is.null(inv)) {
		## if the inverse is available, simply return the inverse matrix
		message("getting cached data")
	} else {
		## If the inverse isn't available, get the original matrix,
		## calculate the inverse, set in the cache, and return the inverse.
		data <- x$get()
		inv <- solve(data, ...)
		x$setinverse(inv)
	}
	inv
}
