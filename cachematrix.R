## Get the inverse of a matrix caching the result for 
## future use.
## We assume that the matrix is inversible.
##
## Example:
## We want to get the inverse of 
## 1 2
## 3 4
## First we must build the special list with the
## cache manipulation functions making a call to
## makeCacheMatrix(), as argument we will pass the
## matrix that we want the inverse of:
## cache <- makeCacheMatrix(matrix(c(1, 3, 2, 4), 2, 2))
## Then we use the cache object with cacheSole:
## cacheSolve(cache)

## Given a matrix it returns a list of functions
## to manipulate the cache. 

makeCacheMatrix <- function(x = matrix()) {
    ## Initialize the var that will hold the inverse.
    m_inv <- NULL
    set <- function(y) {
        ## Replace the original matrix with y and
        ## clean previous inverted matrix.
        x <<- y
        m_inv <<- NULL
    }
    get <- function() {
        ## Get the current original matrix. The one that we
        ## wan tot invert.
        x
    }
    setinv <- function(inv) {
        ## Cache the inverse.
        m_inv <<- inv
    }
    getinv <- function() {
        ## Get the cached inverted matrix.
        m_inv
    }
    ## Return a list of functions to manipulate
    ## the cache.
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Return the inverse of the matrix that is stored
## in x. x is speial a list of functions that acts like
## a cache. You need to create x using makeMatrixCache(matrix)

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    if(is.null(x$getinv())) {
        ## Inverse matrix is not in the cache, so we will
        ## use solve to get the inverse and store it in the cache.
        x$setinv(solve(x$get()))
    }
    
    ## Return the cached inverse.
    x$getinv()
}
