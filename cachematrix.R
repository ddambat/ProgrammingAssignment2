## This is a function to compute the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  k <- NULL
  set <- function(y){
    x <<- y
    k <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) k <<- inverse
  getInverse <- function() k 
  list(set = set, get = get, 
    setInverse = setInverse, 
    getInverse = getInverse)
}


## This is a function to compute the inverse of the matrix produced by makeCacheMatrix

cacheSolve <- function(x, ...) {
  k <- x$getInverse()
  if(!is.null(k)){
    message("getting cached data")
    return(k)
  }
  mat <- x$get()
  k <- solve(mat,...)
  x$setInverse(k)
  k
}