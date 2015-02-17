# A cacheable matrix mini-library

# Example usage:
#
# > m <- matrix(1:4, nrow=2, ncol=2)
# > cm <- makeCacheMatrix(m)
# > cacheSolve(cm)
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(cm)
# getting cached data
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

# Create a cacheable matrix with associated functions to access it
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv) 
}

# Return the inverse of the cacheable matrix
# If the inverse has been cached return the cached matrix
cacheSolve <- function(x) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinv(i)
  i
}
