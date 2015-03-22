## Creates a special matrix that can cache its invers and the second 
#retrieves the inverse if it has already been calculated or computes the 
#inverse if the matrix is changed 

## This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  set <- function(y) {
  x <<- y
  m <<- NULL
}

get <- function() x
setmatrix <- function(solve) m <<- solve
getmatrix <- function() m
list(set = set, get = get,
     setmatrix = setmatrix,
     getmatrix = getmatrix)
}
#This function computes the inverse of the special "matrix" 
#If the inverse has already been calculated (and the matrix has not changed)
#then the cachesolve retrieves the inverse from the cache
cacheSolve <- function(x, ...) {
       
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setmatrix(m)
  m
}

