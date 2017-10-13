## These functions are used to create a special object that stores a ## matrix and caches its inverse

## This Function creates a special "matrix" that caches its inverse        


makeCacheMatrix <- function(x = matrix()) {
		inv <- NULL
		set <- function(y) {
			x <<- y
			inv <<- NULL 
		}
		get <- function() x
		setInverse <- function(inverse) inv <<- inverse
		getInverse <- function() inv
		list(set = set,
			 get = get,
			 setInverse = setInverse,
			 getInverse = getInverse)
}


## This function calculates the inverse of the special "matrix" created ## by the makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
        	message("getting cached data")
        	return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
