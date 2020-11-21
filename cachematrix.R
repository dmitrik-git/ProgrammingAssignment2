## Good explanation of the lexical scoping 
## https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md

## makeCacheMatrix creates a special "matrix" list object which contains the following elements:
## x - the original input matrix to be inversed
## invx - the value of the inversed matrix
## set - a nested function to replace the original input x with a new matrix
## get - a nested function to read the value of the original input matrix
## setinvx - a nested function to store the cached value
## getinvx - a nested function to read the cached value
##
## the final line list() is craeting the list object with named variables for the purpose of using the $ sign 
## when calling the list arguments

makeCacheMatrix <- function(x = matrix()) {
  invx <- NULL
  set <- function(y) {
    x <<- y
    invx <<- NULL
  }
  get <- function() x
  setinvx <- function(inv) invx <<- inv
  getinvx <- function() invx
  list(set = set, get = get,
       setinvx = setinvx,
       getinvx = getinvx)

}


## cacheSolve takes a special list object as input created by the makeCacheMatrix function
## First, the function checks if the object alraedy has a previously cached invx value.
## If so, the value is returned.
##
## Otherwise, the original matrix is fed in, inversed, and the value is stored in invx

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invx <- x$getinvx()
  if(!is.null(invx)) {
    message("getting cached data")
    return(invx)
  }
  data <- x$get()
  invx <- solve(data, ...)
  x$setinvx(invx)
  invx
}
