## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix function creates a special matrix from a regular invertible matrix, which can be then used
## by the cacheSolve function to return the inverse of that matrix + cache the inverse results

## Write a short comment describing this function

#The first function, makeCacheMatrix creates a special "matrix", which is really a list containing 
# a function to

# set the value of the matrix
# get the value of the matrix
# set the value of the inverse matrix
# get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  ## begins by setting the inverse to NULL as a placeholder for a future value
  matrix_inverse <- NULL 
  
  ## defines a function to set the matrix, x, to a new matrix, y, and resets the inverse, 
  ## matrix_inverse, to NULL
  set <- function(y) { 
    x <<- y                 
    matrix_inverse <<- NULL  
  }
  
  ## returns the matrix x
  get <- function() x 
  
  ## sets the matrix_inverse to inverse that was cacluated
  setinverse <- function(inverse) matrix_inverse <<- inverse 
  
  ## returns the inverse matrix_inverse
  getinverse <- function() matrix_inverse 
  
  ## returns the 'special matrix' containing all of the functions just defined
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function

# The following function calculates the inverse of the special "matrix" created with the above 
# function. However, it first checks to see if the inverse has already been calculated. If so, it 
# gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse
# of the data and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  matrix_inverse <- x$getinverse()
  
  ## check if inverse already exists, if yes, return cached value
  if(!is.null(matrix_inverse)) {
    message("getting cached data")
    return(matrix_inverse)
  }
  
  ## Else get the data for the new matrix
  data <- x$get()
  
  ## Find it's inverse
  matrix_inverse <- solve(data, ...)
  
  ## Set the inverse value calcuated for future cache use
  x$setinverse(matrix_inverse)
  
  ## Return the inverse for current run
  matrix_inverse
}

