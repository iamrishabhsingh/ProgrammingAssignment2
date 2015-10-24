# Matrix inversion, is frequently a costly computation and there might be some benefit
# to caching the inverse of a matrix rather than computing it again and again. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
set <- function(y) {
  x <<- y
  m <<- NULL
}
get <- function() x
setmean <- function(mean) m <<- mean
getmean <- function() m
list(set = set, get = get,
     setmean = setmean,
     getmean = getmean)

}


# The following function,calculates and returns the inverse of matrix. It first checks if
# the inverse has already been computed.
# This function works with the assumption that the matrix is invertible.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
