###############################################################
## Functions for caching the inverse of a Matrix
##
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function( m = matrix() ) {
    
    ## First we initialize the inverse property
    i <- NULL
    
    ## Setting the matrix
    set <- function( matrix ) {
        m <<- matrix
        i <<- NULL
    }
    
    ## Get the matrix
    get <- function() {
        ## Return the matrix
        m
    }
    
    ## Setting the inverse of the matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }
    
    ## Getting the inverse of the matrix
    getInverse <- function() {
        ## Returns the inverse property
        i
    }
    
    ## Returns a list of methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    ## Setting its inverse
    m <- x$getInverse()
    
    ## If is not null, returns the inverse
    if( !is.null(m) ) {
        message("Getting cached data")
        return(m)
    }
    
    ## Getting the matrix from our object
    data <- x$get()
    
    ## Calculating its inverse, using matrix multiplication
    m <- solve(data) %*% data
    
    ## Sets the inverse to the object
    x$setInverse(m)
    
    ## Return the matrix
    m
}