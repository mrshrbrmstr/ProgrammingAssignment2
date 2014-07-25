## Memory set aside for storing the values of a matrix x and inverse of x (inv) will remain
## even after the function terminates. 

## Function makeCacheMatrix takes a matrix (default is empty matrix)
## returns a list of 4 objects: set, get, setinverse, getinverse

makeCacheMatrix <- function(x = matrix()) {

      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() { return(x) }
      setinverse <- function(inverse) { inv <<- inverse }
      getinverse <- function() { return(inv) }
      return(list(set = set, get = get, setinverse = setinverse, getinverse = getinverse))
}


## The function cacheSolve will return the cached (stored) inverse of matrix x, if it exists
## If the inverse does not exist, this function will calculate the inverse and store the
## inverse for use in the parent, or global, environment

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getinverse()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
        ## if the value of inv is not empty, its contents are retrieved
        ## this stops the inverse from being calculated again
      data <- x$get()
      inv <- solve(data, ...)
      x$setinverse(inv)
      return(inv)
}
