## This pair of functions works together to cache the inverse of a matrix and store it in memory.

## The "makeCacheMatrix" function takes in a matrix as an argument and then creates a special "matrix" object
## that can cache the input matrix's inverse.

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


## The "cacheSolve" function computes the inverse of the special "matrix" returned by the "makeCacheMatrix" function above. If the inverse has already been calculated
## for a given special "matrix", then "cacheSolve" will retrieve the inverse from the cache and return it, rather than recalculate it.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
