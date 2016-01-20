## Arbee Shadkam


## This function returns a list containing functions to 
##  1. set the matrix 
##  2. get the matrix
##  3. set the inverse
##  4. get the inverse
makeCacheMatrix <- function(x = matrix()) {

  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Return a matrix that is the inverse of 'x' frim cache if available, otherwise, it will compute and put in the cache.

cacheSolve <- function(x, ...) {
        
  inv = x$getinv()
  
  # if the inverse has already been calculated
  if (!is.null(inv)){
    # get it from the cache. 
    return(inv)
  }
  
  # if not found in cache, calculates the inverse 
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  # sets the value of the inverse in the cache.
  x$setinv(inv)
  
  return(inv)
}
