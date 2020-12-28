## makeCacheMatrix and cacheSolve are functions that Inverses Matrix

## makeCacheMatrix provides a matrix that caches its inverse

makeCacheMatrix <- function(z = matrix()) {
  x <- NULL
  set <- function(b) {
    z <<- b
    x <<- NULL
  }
  get <- function() z
  setinverse <- function(inverse) x <<- inverse
  getinverse <- function() x
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve computes the inverse of the matrix reciprocated by makeCacheMatrix

cacheSolve <- function(z, ...) {
  x <- z$getinverse()
  if (!is.null(x)) {
    message("ACQUIRING CACHED DATA")
    return(x)
  }
  data <- z$get()
  x <- solve(data, ...)
  a$setinverse(x)
  x
}

