## the goal of the two functions here it to cache the inverse of a matrix
## in order to avoid to compute the inverse of the same metrics repeatedly
## note: the matrix must be invertible, otherwise an error is returned

## example of usage:
##    x <- matrix(c(1:4),2,2)
##    mmatrix <- makeCacheMatrix()
##    mmatrix$set(x)
##    cacheSolve(mmatrix)


## the function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  ## init: empty inverse matrix
  d = dim(x)
  i <- matrix(NA, nrow = d[1], ncol = d[2])
  
  ## set the matrix content
  set <- function(y){
    x <<- y
    m <<- i
  }
  
  ## get the matrix content
  get <- function() x 
  
  ## set the inverse of the matrix
  ## if it was already computed, it contains the cache
  setinv <- function(invx) m <<- invx
  
  ## get the inverse of the matrix
  getinv <- function() m
  
  ## list containing the functions built
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## the function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## if the inverse has already been calculated and the matrix has not changed, 
## then the cachesolve retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invm <- x$getinv()
  
  ## If the inverse already exists, the function returns it
  if(!all(is.na(invm))) {
    message("getting cached data")
    return(invm)
  }
  
  ## if the inverse does not exist, the function calculates it 
  ## and store it in setinv
  m <- x$get()
  invm <- solve(m)
  x$setinv(invm)
  # print the inverse of the matrix
  invm
}