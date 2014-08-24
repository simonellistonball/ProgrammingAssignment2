## Provides a caching wrapper around the matrix object, and a function
## to calculate and cache the result of the solve() function.

## Usage:
## m<-makeCacheMatrix(X) where X is a normal R matrix object
## cacheSolve(m) will return the solution to the matrix (its inverse)
## a seconds call to cacheSolve(m) will return a cached solution as long 
## as it has not been set to a different matrix in the mean time.

## Creates the wrappers around the basic matrix, providing accessor functions
## set and get to access the underlying matrix, and setsolve and getsolve to 
## provide access to the cached solution. Note that setsolve and getsolve are
## intended for private use, and show not be used directly, use cacheSolve()
## instead.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    # clear the cached result if the underlying matrix changes.
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## Returns the solution to the makeCacheMatrix object x.
## On first invocation, stores the result in the makeCacheMatric object. On 
## further invocations returns the cached version.
## Accepts all arguments of the solve() function, but does not check them 
## for caching purposes.
cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}