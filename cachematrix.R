## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##  This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialize the inverse property
  i <- NULL
  
  ## Set the matrix
  set <- function( y ) {
    x <<- y
    i <<- NULL
  }
  
  ## Get the matrix
  get <- function() x
  
  
  ## Set the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## Get the inverse of the matrix
  getInverse <- function() i
  
  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  
  ## Return the inverse if already in the Cache
  
  if( !is.null(i) ) {
    message("getting cached data")
    return(i)
  }  
  
  data <- x$get()
  
  ## Computing the inverse of a square matrix 
  i <- solve(data) %*% data
  
  x$setInverse(i)
  
  i
}