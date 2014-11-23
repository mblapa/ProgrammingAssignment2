## The following functions allows one to cache
## the inverse of a matrix

## makeCacheMatrix return a list of functions that can cache
## the inverse of a given square matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        getMatrix <- function() x
        setInverse <- function( inv ) i <<- inv
        getInverse <- function() i
        
        list( getMatrix=getMatrix, setInverse=setInverse,
              getInverse=getInverse)   
}


## cacheSolve computes the inverse of a matrix
## If the inverse had been previously calculated, return the inverse from the cache

cacheSolve <- function( functionsList, ... ) {
        m <- functionsList$getInverse()
        if( !is.null(m) ) {
                message("getting cached data")
                return(m)
        }
        
        matrix <- functionsList$getMatrix()
        inv <- solve( matrix )
        functionsList$setInverse( inv )
        inv       
}
