

## Following function creates CacheMatrix
## Input-Matrix
## Output-Special Matrix Object

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(MatrixInverse) inv <<- MatrixInverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


##Following function returns Cached Inverse Matrix or Computes Inverse Matrix when not Cached 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  MatInv <-x$getinv()
  if(!is.null(MatInv)){
    message("Reading Cached matrix Inverse")
    return(MatInv)
  }
  MatrixData <- x$get()
  MatInv <- solve(MatrixData, ...)
  x$setinv(MatInv)
  MatInv
}