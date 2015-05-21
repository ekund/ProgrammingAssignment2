## A set of functions to create a matrix and cache its inverse
## Inverse will be calculated in a lazy manner - the first time it is needed

## This function creates a special matrix wrapper that contains a matrix
## as well as a list of functions to get and set the inverse function

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
        set <- function(y) {
		x <<- y
                m <<- NULL
	}
 	get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function will return the inverse of x,
## using the cached value if available
## x should be the matrix wrapper created via
## makeCachedMatrix

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
