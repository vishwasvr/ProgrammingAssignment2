##Programming Assignment 2

##In this assignement, two function are created to make use of caching 
##functionality for potential time consuming computation of a Matrix Inverse 
##and use the cached solution if available, rather than computing the inverse
##everytime it is called.

##makeCacheMatrix function 
##This function creates a list containg functions to 
##  1. set the value of the matrix
##  2. get the value of the matrix
##  3. set the inverse of the matrix
##  4. get the inverse of the matrix

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


##cacheSolve function
##This function first checks if the inverse of the given matrix is available 
##in cache, if it is then it returns the cached inverse of the matrix along 
##with a message. Else, it calculates the inverse, stores in cache and returns 
##the value. Any subsequent calls for the inverse of the same matrix, the 
##solution will be retrieved from cached data. If the matrix changes from the 
##previous call, then set function needs to be called to reset the matrix.

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

