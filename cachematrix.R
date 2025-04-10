## makeCacheMatrix: creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # this will hold the cached inverse
    
    # Method to set the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL  # reset inverse when matrix changes
    }
    
    # Method to get the matrix
    get <- function() x
    
    # Method to set the inverse
    setinverse <- function(inverse) inv <<- inverse
    
    # Method to get the inverse
    getinverse <- function() inv
    
    # Return list of all methods
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve: computes the inverse of the matrix from makeCacheMatrix
## If the inverse is already cached, it retrieves it instead of recomputing.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    
    # If inverse is already cached, return it
    if (!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    
    # Otherwise, compute inverse and cache it
    mat <- x$get()
    inv <- solve(mat, ...)  # matrix inversion
    x$setinverse(inv)
    inv
}
