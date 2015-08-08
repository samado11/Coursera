## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  # Set is a function that changes the vector.
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  # Setmatrix is a function to store matrix into i
  setmatrix <- function(solve) i <<- solve
  getmatrix <- function() i
  list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix) 
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getmatrix()
  
  #If the inverse already is in cache, skip the computation.
  if(!is.null(i)) { 
    message("getting cached data")
    return(i)
  }
  #If not, calculates the inverse
  matrix <- x$get()
  i <- solve(matrix, ...)
  
  x$setmatrix(i) #set the value of inverse by setmatrix function
  return(i)
}
