## Put comments here that give an overall description of what your
## functions do
Creates a special "vector", which is really a list containing a function to
1. set the value of the matrix
2. get the value of the matrix
3. set the value of the inverse
4. get the value of the inverse

## Purpose: Create a matrix object, that can cache its inverse 
## The part below creates a matrix that caches its inverse. Hereby x is a function argument; m is created in the makeCacheMatrix environment and set at NULL.
# set is also created in the makeCacheMatrix environment and creates a function (y), wherein  both x and m are placed in the parent environment, here the input is set in x and m is set at NULL, reseting any value that may have been present for m in the global environment.
# besides m and s, get, setinverse, getinverse and list are created in the makeCacheMatrix environment. 
# In get a function is created that retrieves x from the parent environment of makeCacheMatrix.  
# In setinverse a function is created, whereby the inverse is set as m, and m is defined in the parent enviroment (meaning the inverse is set by retrieving m from the parent environment)
# In getinverse m - being the inverse of x is retrieved. 
# Finally in list all of the functions are asigned as an element and returned to the parent environment


makeCacheMatrix <- function(x = matrix()) {
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
    
  }
} 
  
## Purpose: Cache inverse
# Cache inverse, by creating function cacheSolve in which a matrix is returned that is the inverse of x
# cacheSolve starts with a function, with argument x and ellipses in which extra arguments can be placed.
  # Within the function m tries to retrieve the inverse of x by x$getinverse
  # if there is no result (aka m is NULL), cached data is retrieved for m and returned to the parent environment  
  # next, the inverse of x is calculated and set
  
  cacheInverse <- function(x, ...) {
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