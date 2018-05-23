## The makeCacheMatrix() function takes a matrix as an argument
## and caches the inverse of that matrix.


makeCacheMatrix <- function(x = matrix()) {
  
  ##  x is a value of a square invertible matrix
  ## Returns a list of contained functions that:
  ##        Sets the matrix
  ##        Gets the matrix
  ##        Sets the inverse of the matrix
  ##        Gets the inverse of the matrix
  ##    The output of this function is the input to the cacheSolve() function
  
         inv_mat <- NULL
         set <- function(y) {
           
           x <<- y
           inv_mat <<- NULL
      
    ## This puts the matrix in the cache            
         }  
         get <- function() x
         setinverse <- function(inverse) inv_mat <<- inverse
         getinverse <- function() inv_mat
         list(set=set, get=get, setinverse = setinverse, getinverse = getinverse)

}


## Computes inverse of the output of the makeCacheMatrix() function.
## If the matrix has not changed, then cacheSolve() will retrieve the matrix
## from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inverse <- x$getinverse()
      if(!is.null(inverse))
        {
        message("getting cached data")
        return(inverse)
      }
      mat_data <- x$get()
      inverse <- solve(mat_data, ...)
      x$setinverse(inverse)
      return(inverse)
}
