####################
## makeCacheMatrix #
#################################################################################
## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.
## This function creates a special "matrix" object that can cache its inverse.
## There is a list of functions: set, get, setInverse and getInverse.
#################################################################################
makeCacheMatrix <- function(x = matrix()) {

        invM <- NULL
		
        set <- function(y) {                                    # set matrix x
                x <<- y
                invM <<- NULL
        }
		
        get <- function() x                                     # get matrix x
        setInv <- function(invMatrix) invM <<- invMatrix        # set inverse matrix
        getInv <- function() invM                               # get inverse matrix
        list(set = set,
		     get = get,
             setInv = setInv,
             getInv = getInv )
			 
}

###############
## cacheSolve #
################################################################################
## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.
## Otherwise, use solve to get inverse matrix, put it into cache and return the
## inverse matrix.
################################################################################
cacheSolve <- function(x, ...) {

		## Return a matrix that is the inverse of 'x'
        invM <- x$getInv()
		
		## if it's in cache, return the inverse matrix
        if(!is.null(invM)) {
                message("getting cached inverse matrix")
                return(invM)
        }
        
        ## Otherwise, use solve to get the inverse matrix
        m <- x$get()                    # get matrix x
        invM <- solve(m, ...)           # use solve to get inverse matrix	
        x$setInv(invM)                  # put the Inverse matrix into cache
        invM                            # return inverse matrix
}
