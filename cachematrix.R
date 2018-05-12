## In the following, we have a pair of functions 
## that cache the inverse of a matrix.



## The function "makeCacheMatrix" creates a special "matrix" 
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  invMat <- NULL
  
  setMat <- function(y) {x <<- y; invMat <<- NULL}
  getMat <- function() {return (x)}
  setInv <- function(y_inv){invMat<<- y_inv} 
  getInv <- function(){return(invMat)}
  
  return(list(setMat = setMat, getMat = getMat,
              setInv = setInv, getInv = getInv))
}



## This function computes the inverse of the special "matrix". 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  matInv <- x$getInv()
  if(!is.null(matInv)) {
    message("Cached data is available. Getting it ..."); return(matInv)}
  matInv <- solve(x$getMat)
  x$setInv(matInv)
  return(matInv)
}
