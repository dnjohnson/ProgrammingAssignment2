## A collection of functions that can be used to retrieve the inverse of
## a matrix. The inverse will be cached for performance unless the matrix
## is modified.

## Wraps the matrix specified in a list that can be manipualted to cache
## the inverse of that matrix
makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	
	#Set or get the internal matrix, setting should reset the cache
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	
	#Set or get the cached inverse of the matrix
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Solves the inverse of a matrix either by retrieving a pre-cached result
## or by generating the inverse and caching it
cacheSolve <- function(x, ...) {
	## Attempt to return a pre-cached inverse if it exists
	i <- x$getinverse()
	if (!is.null(i)) {
		message("using cache")
		return(i)
	}
	
	## Cached version doesn't exist so solve and then cache it
	data <- x$get()
	i <- solve(data, ...)
	x$setinverse(i)
	i
}
