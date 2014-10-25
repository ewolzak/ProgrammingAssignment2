## Method to cache the inverse of a matrix
## with the purpose of saving calculation time
## it defines 2 functions
##  	makeCacheMatrix to set
##  	cacheSolve to get
## The conditions 
## Matrix is quadratic and invertible has to be checked before

## makeCacheMatrix 
## this function stores a matrix in an own environment
## and defines the methods ( set, get, setinvers, getinvers)
##  usage: specMatrix <- makeCacheMatrix(myMatrix)

makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
		set <- function(y) {
				x <<- y
				m <<- NULL
		}
		get <- function() x
		setinvers <- function (solve) m <<- solve
		getinvers  <- function() m
		# create the list
		list(set = set, get = get,
			 setinvers =setinvers,
			 getinvers = getinvers)
		
}


## cacheSolve calculates the inverse of the special "matrix" created 
## with the function makeCacheMatrix.
## It first checks to see if the inverse has already been calculated. If so, 
## it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the inverse in the cache
## via the setinvers function.
## usage cachedinverse <- cacheSolve( specMatrix)

cacheSolve <- function(x, ...) {
        m <- x$getinvers()
		if(!is.null(m)) {
			 message("getting cached data")
			 return (m)
		}
		message( "now computing and cacheing")
		data <- x$get()
		m <- solve(data, ...)
		x$setinvers(m)
		m
}

