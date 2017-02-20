## makeCacheMatrix and cacheSolve are a pair of functions that 
## allow a user to store an inverted matrix in cache. This should
## improve performance when calculating the inverted matrix is an
## expensive operation

## makeCacheMatrix creates a special type of matrix that can cache
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve is a function that looks to see if an inverted matrix
## is cached. If it is then it returns it. If it is not cached then
## it calculates it anew, stores it in the cache, and returns it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data)
  x$setsolve(s)
  s
}
