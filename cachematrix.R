## Overall, this pair of functions will serve to create a matrix and its
## inverse which can be cached and referenced for later by exploiting R's 
## lexical scoping.


## This function creates a matrix and an object (i) for its cached inverse
## that goes across environments.
## It also holds functions that create access to the environment where the 
## matrix and its inverse exist.


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve uses the functions created in makeCacheMatrix to evaluate
## the value of i to determine if the inverse already exists.
## In the first use of cacheSolve i will be null, but will get created as the matrix inverse
## and cached with the use of the setinverse function. 
## In the second pass of cacheSolve, it will recall the value of i set in the first iteration
## and retrieve that cached value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}