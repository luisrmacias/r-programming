## The function makeCakeMatrix creates a matrix and
## keep its inverse in cache while cacheSolve determines if 
## the inverse of a matrix has been calculated and retrieves it
## from cache if so whereas it calculates the inverse if it is a 
## new matrix 

## Function for matrix creation and cache storing

##the argument for makeCacheMatrix must be an invertible matrix
##store makeCacheMatrix' output in a variable
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
	     getinverse	= getinverse)
}


## Function that determines whether the inverse of a matrix has 
## been calculated and retrieves it from cache if so. In case it is
## a new matrix it calculates its reverse. 

##use the output of makeCacheMatrix as the argument for cacheSolve

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m  <- x$getinverse()
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}
