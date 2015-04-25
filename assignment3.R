
#This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = numeric()) {
        # initially nothing is cached so set it to NULL
        cache <- NULL
        
        # store the matrix
        setMatrix <- function(y) {
                x <<- y
                
                cache <<- NULL
        }

        # returns the stored matrix
        getMatrix <- function() {
                x
        }

        # cache the given argument 
        cacheInverse <- function(solve) {
                cache <<- solve
        }

        # get the cached value
        getInverse <- function() {
                cache
        }
        
        # returns a list
        list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}


# The following function calculates the inverse of a "special" matrix created with 
# makeCacheMatrix
cacheSolve <- function(y, ...) {
        # get the cached value
        inverse <- y$getInverse()
        # return the cache value if exists
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        # else get the matrix, caclulate the inverse and store it in the cache
        data <- y$getMatrix()
        inverse <- solve(data)
        y$cacheInverse(inverse)
        
        # return the inverse
        inverse
}
