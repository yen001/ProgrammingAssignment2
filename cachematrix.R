## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a matrix object,  
## and cacheSolve calculates the inverse of the matrix.
## If the matrix inverse has already been calculated, it will 
## find it in the cache and return it. 
## Not calculate it again.

makeCacheMatrix <- function(x = numeric()) {
  inv_of_x <- NULL
  set <- function(y) {
    x <<- y
    inv_of_x <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) inv_of_x <<- solve
  getsolve <- function() inv_of_x
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## The function cacheSolve returns the inverse of a matrix created with
## the makeCacheMatrix function.
## If the cached inverse is available, cacheSolve retrieves it, while if
## not, it computes, caches, and returns it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv_of_x <- x$getinverse()
    if (!is.null(inv_x)) {
        message("here is cached inverse matrix")
        return(inv_of_x)
    } else {
        inv_of_x <- solve(x$get())
        x$setinverse(inv_of_x)
        return(inv_of_x)
    }
}


 

