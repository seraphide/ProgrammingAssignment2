## The objective of the makeCacheMatrix and cacheSolve functions are to take
## a matrix and cache it and its inverse. From an object made with 
## makeCacheMatrix, a matrix and its inverse can be obtained and set.

## The makeCacheMatrix function takes an argument, x, as a matrix and sets the
## object to hold the argument via the internal set function. This value can be
## called via the get function. Likewise, analogous operations can be performed
## with the inverse using the setinverse and getinverse functions.

makeCacheMatrix <- function(x = matrix()) {
		inv <- NULL
		set <- function(y) {
				x <<- y
				inv <<- NULL
		}
		get <- function() x
		setinverse <- function(inverse) inv <<- inverse
		getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The cacheSolve function takes an argument, x, and checks to see if the
## inverse of its stored matrix is available. If available, it is returned, else
## the inverse of the matrix in x is computed via solve() and is then set to x.

cacheSolve <- function(x, ...) {
		inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}
