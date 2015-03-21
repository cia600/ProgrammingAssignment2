## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function( m = matrix() ) {
  
  ## Initialize the inverse matrix
  i <- NULL
  
  ## Sets the matrix anew but outside of the function
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  ## Gets the matrix (searches 1st in function, then ourside)
  get <- function() {
    ## Return the matrix
    m
  }
  
  ## Sets the inverse of the matrix outside of the function
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## Gets the inverse of the matrix
  getInverse <- function() {
    i
  }
  
  ## Creates the output, a list of sub functions to be called later
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## Gets a matrix that is the inverse of 'x' 
  m <- x$getInverse()
  
  ## Check if the inverse has been set and return it
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## Else, get the original matrix...
  data <- x$get()
  
  ## ...Inverse it 
  m <- solve(data) %*% data
  
  ## Set this value as the new inverse to be stored outside of the function
  x$setInverse(m)
  
  ## Return the matrix
  m
}
