

## This function, creates a special matrix, which is actually a list containing functions to,
## get the elements of matrix, set the elements of matrix, get the inverse of the matrix,
## set the inverse of the matrix (i.e. caching).

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(i) inv <<- i
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function calculates inverse of the matrix, it however first searches in the cache 
## for the inverse of the matrix, before calculating it. If found it returns that inverse,
## else it calculates the inverse, stores in the cache and returns the inverse using the solve function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setinverse(inv)
  inv
}
