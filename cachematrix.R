## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  iMat <- NULL
  set <- function(matY) {
    x <<- matY
    iMat <<- NULL
  }
  get <- function() x
  setMatInv <- function(inverse) iMat <<- inverse
  getMatInv <- function() iMat
  list(set = set, get = get,
       getMatInv = getMatInv,
       setMatInv = setMatInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
iMat <- x$getMatInv()
  if (!is.null(iMat)) {
      message("getting cached data")
      return(iMat)
  }
  fMat <- x$get()
  iMat <- solve(fMat, ...)
  x$setMatInv(iMat)
  iMat
  }
