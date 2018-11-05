## The two following functions are used to create a special object that 
## stores a matrix and cache its inverse. 

## The first function, makeCacheMatrix creates a special "matrix", which is really a
## list containing a function to set the value of the matrix, get the value of the matrix,
## set the value of the inverse and get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
      ## Initialize the inverse
      inv <- NULL
      ## Set the matrix
      set <- function(y) {
            m <<- y
            inv <<- NULL
      }
      ## Get the matrix
      get <- function() {
            m
      }
      ## Set the inverse of the matrix
      set_inverse <- function(inverse) {
            inv <<- inverse
      }
      ## Get the inverse of the matrix
      get_inverse <- function() {
            inv
      }
      ## Return a list with these values
      list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## The following function calculates the inverse of the special "matrix" created with
## the above function. However, it first checks to see if the inverse has already been
## calculated. If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in 
## the cache via the set_inverse function.

cacheSolve <- function(x, ...) {
      ## Get the inverse of the matrix
      i <- x$get_inverse()
      ## Test if the inverse has already been calculated
      if(!is.null(i)) {
            message('getting cached data')
            return(i)
      }
      ## Get the matrix from the list
      data <- x$get()
      ## Calculate the inverse
      i <- solve(data,...)
      ## Set the inverse to the list
      x$set_inverse(i)
      ## Return the inverse
      i
}