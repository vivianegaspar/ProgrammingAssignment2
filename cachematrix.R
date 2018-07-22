makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  #  function to set the value of the matrix and clears cache
  set <- function(y) {
    x <<- y    # Set the value in parent environment
    m <<- NULL # Clear cache
  }
  #to get the value of the matrix
  get <- function() x
  #to set the inverse
  setInverse <- function(inverse) m <<- inverse
  #function to get the inverse
  getInverse <- function() m
  
  # Return a list with the 4 functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# this function calculates the inverse of the matrix returned by 
# makeCacheMatrix above. If the inverse is already calculated and the matrix is the same,
#the cachesolve retrieves the inverse from the cache

cacheSolve <- function(x) {
  m <- x$getInverse() # get value for the inverse
  if(!is.null(m)) { # If the cache is not empty, return it
    message("getting cached data")
    return(m)
  }
  # when cache is empty, We need to calculate it, cache it, and return it
  data <- x$get()  # get value of matrix
  m <- solve(data) # calculate rhe inverse
  x$setInverse(m)  # cache the result
  m                # return the inverse
}
