## makeCacheMatrix generates a list of functions to create and retreive a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
		inv <- NULL
		set <- function(y) {
			x <<- y
			inv <- NULL
		}
		get <- function() x
		setInv <- function(inverse) inv <<- inverse
		getInv <- function() inv
		list (set=set, get=get,setInv=setInv,getInv=getInv)
}

## cacheSolve retrieves the cached version of the matrix's inverse and creates the inverse if none exist.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        	inv <- x$getInv()
	if (!is.null(inv)) {
		message('Getting the cached inverse!')
		return(x$getInv())
	}
	invMatrix <- solve(x$get())
	x$setInv(invMatrix)
	x$getInv()
}
