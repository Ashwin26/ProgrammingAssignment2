## There are two function defined here - 
## makeCacheMatrix() - is a set of 4 functions that are used to set and get the matrix and its inverse
## cacheSolve() - is a function that returns the inverse of the matrix either from the cache, or by computing it directly
## These functions together highlight the cache feature and show how to use it to optimize your program


## makeCacheMatrix is a function that contains 4 functions - 
## two for getting and setting the matrix
## two for getting and setting the inverse of the matrix
## these functions can get called inside another function (e.g. cacheSolve shown below)

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL				# initializes i - variable for holding the inverse matrix
	setmatrix <- function(y) {	# sets the value of matrix x, and initializes inverse matrix i as NULL
		x <<- y
		i <<- NULL
	}
	getmatrix <- function() x				# prints the value of matrix x
	setinverse <- function(inverse) i <<- inverse	# stores the value of the inverse in i
	getinverse <- function() i				# prints the value of the inverse matrix i
	list(setmatrix = setmatrix, getmatrix = getmatrix,
		setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve is a function that utilizes the cache to return the inverse of a matrix x
## It first searches the cache, and returns the inverse matrix from there if it is available
## Reading from the cache is very fast and this feature is useful for optimizing the efficiency of your algorithm
## If the cache is empty, the function will call the getmatrix() function that was defined in makeCacheMatrix() to get the matrix x
## And then compute its inverse using the solve() function
## It will then call the setinverse() function from makeCacheMatrix() to set the inverse i
## If the cacheSolve function is executed a second time, it will return the inverse from the cache since i has been set

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	i <- x$getinverse() 	# calling getinverse() in case i it has already been computed
	if(!is.null(i)) {
		message("Getting cached inverse matrix...")
		return(i)		# returns the inverse from cache
	}
	message("Calculating inverse matrix... (not cache)")
	data <- x$getmatrix()	# gets the matrix
	i <- solve(data, ...)	# computes its inverse using solve
	x$setinverse(i)		# sets the inverse value using the function (so that next time, the value is returned from cache)
	i				# prints the inverse matrix
}
