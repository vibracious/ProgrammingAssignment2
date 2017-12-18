 
	## calculates the inverse of the matrix.
	## If the matrix inverse has already been calculated, it will instead 
	## find it in the cache and return it, and not calculate it again.
	

	makeCacheMatrix <- function(m = matrix()) {
	    inv_m <- NULL
	    set <- function(n) {
	        m <<- n
	        inv_m <<- NULL
	    }
	    get <- function() m
	    setinverse<- function(inverse) inv_m <<-inverse
	    getinverse <- function() inv_m
	    list(set = set, get = get,
	         setinverse = setinverse,
	         getinverse = getinverse)
	}
	

	## The function cacheSolve returns the inverse of a matrix A created with
	## the makeCacheMatrix function.
	## If the cached inverse is available, cacheSolve retrieves it, while if
	## not, it computes, caches, and returns it.
	cacheSolve <- function(x, ...) {
	    ## Return a matrix that is the inverse of 'x'
	    inv_m <- m$getinverse()
	    if (!is.null(inv_m)) {
	        message("getting cached inverse matrix")
	        return(inv_m)
	    } else {
	        inv_m <- solve(m$get())
	        m$setinverse(inv_m)
	        return(inv_m)
	    }
	}

