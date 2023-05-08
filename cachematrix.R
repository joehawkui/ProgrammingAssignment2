## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# The makeCacheMatrix function creates a list with four objects of other functions.
# These functions are located in the global environment and available to be passed
# to the casheSolve function for computing the inverse of the matrix and storing it
# to the cache.  The first function is set, which stores a new matrix to the variable
# matrix_to_inverse and clears what is in the variable inverse_matrix.  The second
# function is get, which retrieves the current matrix in the variable
# matrix_to_inverse.  The third function is setmatrix, which specifies that
# the variable inverse_matrix is where to store the computed inverse matrix.
# The fourth function is getmatrix, which retrieves the stored value of
# inverse_matrix in the cache.

makeCacheMatrix <- function(matrix_to_inverse = matrix()) {
  inverse_matrix <- NULL
  set <- function(new_matrix) {
      matrix_to_inverse <<- new_matrix
      inverse_matrix <<- NULL
  }
  get <- function() matrix_to_inverse
  setmatrix <- function(solve) inverse_matrix <<- solve
  getmatrix <- function() inverse_matrix
  list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}


## Write a short comment describing this function

# The cacheSolve matrix takes the functions created in the makeCacheMatrix
# function and a matrix to have the inverse computed using the matrix_to_inverse
# variable.  The function first retrieves what is currently stored in the variable
# inverse_matrix in the cache.  If there is something currently in inverse_matrix,
# the function returns that matrix.  If there is not something currently in
# inverse_matrix, the function then looks to the get function from makeCacheMatrix
# and computes the inverse of the matrix in matrix_to_inverse and stores it in
# the variable inverse_matrix using the setmatrix function from makeCacheMatrix.
# It then prints the matrix in the variable inverse_matrix for the user.

cacheSolve <- function(matrix_to_inverse, ...) {
        ## Return a matrix that is the inverse of 'matrix_to_inverse'
  inverse_matrix <- matrix_to_inverse$getmatrix()
  if(!is.null(inverse_matrix)) {
      message("getting cached data")
      return(inverse_matrix)
  }
  data <- matrix_to_inverse$get()
  inverse_matrix <- solve(data, ...)
  matrix_to_inverse$setmatrix(inverse_matrix)
  inverse_matrix
}
