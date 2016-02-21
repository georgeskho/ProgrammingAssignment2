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

        inv <- NULL
		
        set <- function(y) {							# set matrix x
                x <<- y
                inv <<- NULL
        }
		
        get <- function() x							# get matrix x
        setInverse <- function(inverse) inv <<- inverse		                # set inverse matrix
        getInverse <- function() inv						# get inverse matrix
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
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
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("Getting Cached Inverse Matrix")
                return(inv)
        }
        mat <- x$get()									# get matrix x
        inv <- solve(mat, ...)								# use solve to get inverse matrix
        x$setInverse(inv)								# put the Inverse matrix into cache
        inv										# return inverse matrix
}
