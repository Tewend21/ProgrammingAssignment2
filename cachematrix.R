## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix
## Below 2 functions create a matrix and caches its inverse

## The function makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL ## Initially set to null
    ## Set the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x ## Get the matrix
    setInverse <- function(inverse) inv <<- inverse ## Set Matrix inverse
    getInverse <- function() inv ## get matrix inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}

#############################################################################################################

## The function 2.cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above
##  If the inverse has already been calculated, then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    ## If the inverse is already computed and cached, get it
    if(!is.null(inv)) {
        message("getting cached data")
      return(inv)
    }
    ## If not ...
    MyMatrix <- x$get() ## get the matrix
    inv <- solve(MyMatrix, ...) ## Compute its inverse
    x$setInverse(inv) ## Cache it
    inv ## return the result
}
