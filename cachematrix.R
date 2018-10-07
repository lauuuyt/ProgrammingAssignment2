## Put comments here that give an overall description of what your
## functions do
##The funtion computes the inverse of a matrix and caches it, 
##so that the program only needs to make the computatiton once.

## Write a short comment describing this function
##The function creates an object that caches an inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse_matrix <- NULL
    set <- function(y) {
      x <<- y
      inverse_matrix <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inverse_matrix <<- inverse
    getinv <- function() inverse_matrix
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
##The function computes the inverse of the matrix returned by makeCacheMatrix.
#or retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    inverse_matrix <- x$getinv()
    if (!is.null(inverse_matrix)){
      message("getting cached data")
      return(inverse_matrix)
    }
    data <- x$get()
    inverse_matrix <- solve(data, ...) ## Return a matrix that is the inverse of 'x'
    x$setinv(inverse_matrix)
    inverse_matrix
}
