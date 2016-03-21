## The functions below demostrate how lexical scoping works in R

## The function makeCacheMatrix includes functions to get, set the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The function cacheSolve gets the inverse if one already exists for the matrix or
## if one doesnt exist creates the inverse and set the inverse value to be cached

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached matrix")
    return(i)
  }
  mat <- x$get()
  i <- ginv(mat)
  x$setinv(i)
  i
}
