## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCacheMatrix creates a new matrix object and sets its initial values. In addition 
# it adds further functions to the object to set its inverse as a cached value via (setinverse) 
# as well as getting the original value or the inverse value. The final section defines the
# available functions for calling from other functions as we'll see in the cacheSolve function.


makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) m <<- inverse
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)  
}


## Write a short comment describing this function

# cacheSolve beings by accessing the getinverse function of the passed object which on the
# first call will be set to NULL. If it is null it will get the matrix then perform the solve 
# function on the matrix and update the object by using its setinverse function thus caching the
# value and then finally returning the result. Further calls of the function will access the 
# cached inverse value and enter the initial "if statement" which will call the "getting cached data" 
# message and will return the cached value. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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