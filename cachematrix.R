## The first function creates a matrix object, then the next function takes the inverse of the matrix created by the first function.

## This function creates a matrix object.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x<<- y
    m <<- NULL
  }
  get<- function() {x}
  setInverse <- function(inverse) (m <<- inverse)
  getInverse <- function() {m}
  list(set = set, get = get, setInverse =setInverse, getInverse = getInverse)
}


## This function takes the inverse of the matrix created above.

cacheSolve <- function(x, ...){
  m <- x$getInverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  mat <- x$get()
  m <- solve(mat, ...)
  x$setInverse(m)
  m
}

