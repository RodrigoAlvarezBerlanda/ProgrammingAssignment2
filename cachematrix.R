#This function assummes that array supplied is inverted. If not the function will
#return a NULL value.

makeCacheMatrix <- function(x= matrix()){
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
#Here we get the values of the matrix, the values of the inverse using a double error
#operator (<<). After we get the values of the inverse a list is created including
#these values.
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
#This function computes the inverse values of the matrix. First this check if
#the matrix has already been inverted, if not it will get the inverse.

cacheSolve <- function(x, ...){
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
  
}
