## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function( mat =matrix() ) {
	
	## Initializing the inverse property
		inv<- NULL

	## Setting the matrix
	set <- function( matrix ) {
		mat<<- matrix
		inv<<- NULL
	}

	## Getting the matrix
	get <- function() {	
		## Return the matrix
		mat
	}
	
	## Method which sets the inverse of the matrix
	setInverse <- function(inverse) {
		inv <<- inverse
	}
	
	## Method which returns the inverse of the matrix
	getInverse <- function() { 
		inv
	}
	
	## Return a list of the methods
	list(set = set, get = get,
	setInverse = setInverse,
	getInverse = getInverse)}
		
## If the inverse of the matrix is already determined then the "cachesolve" will get the 	
## inverse from the cache.
		
cacheSolve <- function(x, ...) {
	
	## Return a matrix that is the inverse of 'x'
	mat <- x$getInverse()
		
	## If it is set then fetch the inverse
	if( !is.null(mat) ) {
		message("Fetching cached data")
		return(mat)
	}
		
	## Receive the matrix from our object
	receivedData <- x$get()
	
	## Perform matrix multiplication to get the inverse
	mat <- solve(receivedData) %*% receivedData
	
	## Setting the inverse to the object
	x$setInverse(mat)
	
	## Return the matrix
	mat
}