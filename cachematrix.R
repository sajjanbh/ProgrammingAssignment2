## The main objective of these functions is to calculate the inverse of a matrix
## and then to cache the solution in order to prevent unnecessary processing and speed up operations.

## The function 'makeMatrix()' takes the matrix as an input argument, calculates the
## inverse of the given matrix and caches it. This function outputs a list of four components.

makeMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}

	get <- function() x
	setinverse <- function(inverse) i <<- solve(x)
	getinverse <- function() i
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The function 'cacheinverse()' extracts the inverse matrix and original matrix from 
## the list outputted by makeMatrix() function. It evaluates the inverse matrix if
## it hasn't already been cached and the matrix hasn't been changed.

cacheinverse <- function(x, ...) {
	i <- x$getinverse()
	if(!is.null(i) && x$get() == solve(i)) {
		message("Getting cached data")
		return(i)
	}

	data <- x$get()
	i <- solve(data, ...)
	x$setinverse(i)
	i
}
