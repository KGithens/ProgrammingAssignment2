## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#will create a matrix and expose three methods to set/get it (x) and it's inverse (m)

makeCacheMatrix <- function(x = matrix()) {
  #m is Inverse
  m <- NULL
  #set x in the parent env - if m is already set unset it.
  set <- function(y = matrix()) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  #set the inverse variable, return it
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
# Will check to see if there is already a cached inverse and return it.
# If there is not a cached inverse, it will solve and return it.
cacheSolve <- function(x) {
  m <- x$getInverse()
  
  #if inverse is cached (and it is a matrix) return it
  if(!is.null(m) && is.matrix(m)) { 
    return(m)
  }
  
  #inverse is not cached, we  need to solve it
  data <- x$get()
  m <- solve(data)
  
  #set the value of the inverse
  x$setInverse(m)
  m
}
