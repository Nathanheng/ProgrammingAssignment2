## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  cached_inverse <- NULL
  
  list(
    
    set = function(new_matrix) {
      x <<- new_matrix
      cached_inverse <<- NULL
    },
    
    get = function() {
      x
    },
    
    setinverse = function(inv) {
      cached_inverse <<- inv
    },
    
    getinverse = function() {
      cached_inverse
    }
    
  )
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  # Check if inverse already exists
  inverse <- x$getinverse()
  
  if (!is.null(inverse)) {
    message("getting cached inverse")
    return(inverse)
  }
  
  # Otherwise compute it
  matrix_data <- x$get()
  inverse <- solve(matrix_data, ...)
  
  # Store it
  x$setinverse(inverse)
  
  inverse
}
