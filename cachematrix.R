## Example of use cached results of processing when source data hasn't change.


## This function creates a special "matrix" object that can cache its inverse.
## makes a list of special functions (like methods of object)

makeCacheMatrix <- function(x = matrix()) {
  inv.matrix <- NULL
  # 'set' - assigns matrix to 'x', resets cashed inversion 
  # (because source of inversion is changed)
  set <- function (y = matrix()) {
    x <<- y
    inv.matrix <<- NULL
  }
  # 'get' returns matrix
  get <- function () x
  
  # 'setinvert' remembers inverted matrix in cache
  setinvert <- function (inv) inv.matrix <<- inv
  
  # 'getinvert' returns inverted matrix from cache
  getinvert <- function() inv.matrix
  
  # return a list as result of makeCacheMatrix
  list (set = set, get = get, setinvert = setinvert, getinvert = getinvert)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  # try to get invers from cache
  inv <- x$getinvert()
  if (!is.null(inv)) { # it's there! return cached value
    message ("getting cached data")
    return (inv)
  }
  
  # if we went here, cache is empty, we have to invert matrix ourselves
  matr = x$get()
  inv = solve(matr)
  x$setinvert(inv)
  inv
}
