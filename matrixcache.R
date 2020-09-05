## The first function, makeCacheMatrix creates a special matrix, which is really a list containing a function to
##
##1. set the value of the matrix
##2. get the value of the matrix
##3. set the value of the inverse of matrix
##4. get the value of the inverse of matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL 
  }
  get <- function() x
  setInverseMatrix <- function(inverseMatrix) inv <<- inverseMatrix
  getInverseMatrix <- function() inv
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}


## The following function calculates the inverse of the special "matrix" created with the above function.
## It first checks to see if the inverse  has already been calculated.
cacheSolve <- function(x, ...) {
  inverseMatrix <- x$getInverseMatrix()
  if(!is.null(inverseMatrix)) {
    inverseMatrix
  }
  matrix <- x$get()
  inverseMatrix <- solve(matrix)
  x$setInverseMatrix(inverseMatrix)
  inverseMatrix
}