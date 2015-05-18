## You can run unit test using `devtools::test()`

## This function creates a special "matrix" object
## that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  ##When created the cache is empty
  cache <- NULL
  ##The set function set a new value to this object and
  ##Invalidates the cache
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  ##The get function returns the vanilla value
  get <- function() x
  ##This function  Set the inverse in the cache
  setInverse <- function(inverse) cache <<- inverse
  ##This function retrive the cache of the object
  getInverse <- function() cache
  ##Construct a object with the function members
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x
  ##First check if something in the cache
  m <- x$getInverse()
  if(!is.null(m)) {
    ##Cache hit , return the cached value
    message("getting cached data")
    return(m)
  }
  ##No cache value, get the original matrix
  data <- x$get()
  ##Compute the result
  m <- solve(data) 
  ## Save the result in the cache
  x$setInverse(m)
  ##Return the inverse
  m
}
