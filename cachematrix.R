# makeCacheMatrix creates a special "matrix", which is really a list containing a function to:
# 1) set the value of the matrix
# 2) set the value of the matrix
# 3) set the value of the inverse
# 4) set the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      
      get <- function() x
      setInverse <- function(inverse) i <<- inverse
      getInverse <- function() i
      list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# cacheSolve function calculates the inverse of the special "matrix".
# It first checks to see if the inverse has already been calculated.
# If so, it gets the inverse from the cache and skips the computation.
# Otherwise, it calculates the inverse of the data
# and sets the value of the inverse from the cache.

cacheSolve <- function(x, ...) {
  
      # Return a matrix that is the inverse of 'x'
  
      i <- x$getInverse()
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setInverse(i)
      i
        
}
