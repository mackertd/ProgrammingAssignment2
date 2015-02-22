## Stores a copy of the original matrix and the inverse of the matrix once computed

## makeCacheMatrix

## - Get the value of the matrix
## - Set the value of the matrix
## - Get the inverse of the matrix
## - Set the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      
      ## Set the inverse to null
      
      inverse <- NULL
      
      ## Set the value of the matrix; non-inversed
      
      set <- function(y) {
            x <<- y
            inverse <<- NULL
      }
      
      ## Get the value of the orginal matrix
      
      get <- function() x
      
      ## Set the inverse of the matrix
      
      setInverse <- function(tempInverse) inverse <<- tempInverse
      
      ## Get the inverse of the matrix
      
      getInverse <- function() inverse
      
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## cacheSolve takes a matrix and attempts to retrieve a cache of the inverse of the matrix
## If an inverse of the matrix is not present it will compute an inverse of the matrix and 
## store the inverse

cacheSolve <- function(x, ...) {
      
      ## Return a matrix that is the inverse of 'x'
      
      inverse <- x$getInverse()
      
      if(!is.null(inverse)) {
            message("Getting cached data.")
            return(inverse)
      }
      
      # Get the original matrix
      
      data <- x$get()
      
      # Invert the matrix
      
      inverse <- solve(data)
      
      # Store the inverted matrix
      
      x$setInverse(inverse)
      
      inverse
      
}
