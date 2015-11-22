## Put comments here that give an overall description of what your
## functions do
## These two functions work together to return the inverse of a provided matrix.
## However, since it can be an intensive computation, it will cache the result.
## The cached result is returned the next time it is run to save time.


## Write a short comment describing this function
## makeCacheMatrix retuns a list of 4 different functions.
## These functions are used to retrieve and set the Inverse of the Matrix

makeCacheMatrix <- function(x = matrix()) {

      s <- NULL
      set <- function(y) {
            x <<- y
            s <<- NULL
      }
      get <- function() x
      setInverse <- function(solve) s <<- solve
      getInverse <- function() s
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
      
}


## Write a short comment describing this function
## cacheSolve will return the inverse of a matrix when you pass it the list
## created by makeCacheMatrix. Before it calculates the inverse, it will
## check to see if there is a cached solution first.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

      s <- x$getInverse()
      if(!is.null(s)) {
            message("getting cached data")
            return(s)
      }
      data <- x$get()
      s <- solve(data, ...)
      x$setInverse(s)
      s
      
}
