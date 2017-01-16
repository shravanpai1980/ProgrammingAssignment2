## Creating a matrixobject,that can take inverse. 
## As in example provided in question, I will create a list of functions that can
## Get and set values of matrix and get its inverse

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set 	  <- function(y) {
		x 	  <<- y    ## This changes value of matrix in the global environment
		inverse <<- NULL ## When a new matrix is input, inverse changes to NULL
	}
	get 		<- function() x
	getInverse	<- function() inverse
	setInverse	<- function(value) inverse <<- value
	list(set = set, get = get, getInverse = getInverse, setInverse = setInverse)
}

## CacheSolve function calculates the inverse of matrix only if it is NULL
## If it is not NULL, then it pulls it from Cache
## If a new matrix is input then inverseis set to NULL and hence will be calculated

CacheSolve <- function(x,...) {
	a <- x$getInverse()
	if (!is.null(a)) { ## In case of prior calculated value of inverse
		message("calculating from cache")
		return(a)
	}
	data 		<- x$get() 		##Take value of new matrix
	a		<- solve(data)	##Calculate inverse
	x$setInverse(a)			##Change value of inverse to this new value
	a
}	
