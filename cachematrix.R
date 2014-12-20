## My functions will using R lexical scoping to store the inverse of matrix in Cache for future use
##  it will save the future computations if I will need the inverse of same matrix 

## makeCacheMatrix is a simple getter and setter for my matrix and its inverse 
##to store the  values in cache 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse 
  getInverse <- function() inv 
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


##this function checks for the matrix inverse in cache , if found return its value from 
## the cache ,otherwise calculate the matrix inverse , stores in the cache and returns its value

cacheSolve <- function(func, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- func$getInverse() 
  if( !is.null(inv) ){
    message("getting cached data")
    return (inv) 
  }
  data <- func$get()
  inv <- solve(data, ...)
  func$setInverse(inv)
  return (inv) 
}
