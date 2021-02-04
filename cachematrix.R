## Put comments here that give an overall description of what your
## functions do
Creates a special "vector", which is really a list containing a function to
1. set the value of the matrix
2. get the value of the matrix
3. set the value of the inverse
4. get the value of the inverse

## Create a matrix object, that can cache its inverse 

makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

## Cache inverse

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
