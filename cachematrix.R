
## makeCacheMatrix function set the value of the matrix, then get the value of the matrix.
## Then it set the value of matrix inverse, then get the value of matrix inverse.


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list (set = set, get = get, setinverse = setinverse,
        getinverse = getinverse)
}


## The function below will check wether the inverse has been calculated. If so, it get the value and skips the calculation.
## If not, then the function will work on calculating the inverse of data and set the value of inverse in the cache.

cacheSolve <- function(x=matrix(), ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    massage("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
  ## Return a matrix that is the inverse of 'x'
}