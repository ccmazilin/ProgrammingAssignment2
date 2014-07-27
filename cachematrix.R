## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a list of functions:
## get: get the matrix
## set: set the matrix
## getInv: get the calculated inv matrix
## setInv: calculate the inv matrix by @solve()@
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ##define getter of original matrix
  get <- function() x
  ##define a method to retrieve the inv matrix
  setInv <- function(mean) m <<- solve(x)
  ##define a method to retrieve the inv matrix
  getInv <- function() m

  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## this method is to compute the inv matrix,
## only if the matrix has not been computed
## if it has been computed, return the computed value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()
  
  ##if the inv matrix has been calculated,
  ##return (and end) the method
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ##if not, calculate the inv matrix then return it.
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  m
}
