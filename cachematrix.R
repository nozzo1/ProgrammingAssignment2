## This function processes the input matrix parameters and outputs to x
## it also contains two further functions below.

makeCacheMatrix <- function(x = matrix()) {
  iMat <- NULL
  set <- function(matY) {
    # note the use of the double arrow to save data to the parent environment.
    x <<- matY
    iMat <<- NULL
  }
  get <- function() x
  # setup the Set Matrix Inverse function
  setMatInv <- function(inverse) iMat <<- inverse
  # setup the Get Matrix Inverse function
  getMatInv <- function() iMat
  # now initialise everything above.
  list(set = set, get = get,
       getMatInv = getMatInv,
       setMatInv = setMatInv)
}


## here we use the various functions previously set to produce
## an inverse matrix and store the output in to our cache 'iMat'.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' and save to iMat
iMat <- x$getMatInv()
  ## now test iMat for cached data or if it is null.
  if (!is.null(iMat)) {
      message("getting cached data")
      return(iMat)
  }
  ## output matrix to fMat
  fMat <- x$get()
  ## solve matrix and store in iMat
  iMat <- solve(fMat, ...)
  ## use the inverse function to store inverse of matrix iMat
  x$setMatInv(iMat)
  ## output iMat
  iMat
  }
