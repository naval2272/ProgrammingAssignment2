## Function to create matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i  <- NULL
  set  <- function(y) {
    x <<- y
    i <<- NULL 
  }
  get  <- function() x
  setInverse  <- function(inverse) i  <<- inverse
  getInverse  <- function() i
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
  
}


## Function to compute the inverse. If the inverse has been already been
## calculated, then cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  i  <- x$getInverse()
  if (!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data  <- x$get()
  i  <- solve(data, ...)
  x$setInverse(i)
  i
}
