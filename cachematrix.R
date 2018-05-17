## Put comments here that give an overall description of what your
## functions do

# Matrix inversion is usually a costly computation and their may be some
# benefit to caching the inverse of a matrix rather than compute it
# repeatedly. The following two functions, 'makeCacheMatrix' and 'cacheSolve', 
# will create a special "matrix" object that can cache its inverse and compute 
# the inverse of the special "matrix" returned by `makeCacheMatrix` above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the `cachesolve` should retrieve the inverse from the cache.

## Write a short comment describing this function

# The first function, `makeCacheMatrix` creates a special "matrix", which is
# really a matrix containing a function to
# 
# 1.  set the value of the matrix
# 2.  get the value of the matrix
# 3.  set the value of the inverse of the matrix
# 4.  get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

# The following function calculates the inverse of the special "matrix"
# created with the above function. However, it first checks to see if the
# the inverse has already been calculated. If so, it `get`s the inverse from the
# cache and skips the computation. Otherwise, it calculates the inverse of
# the matrix and sets the value of the inverse in the cache via the `setinverse`
# function.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    return(inv)
}
