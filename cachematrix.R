## The makeCacheMatrix function creates a special "matrix" to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setMatrix <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getMatrix <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}



## The cacheSolve function computes the inverse of the special "matrix" created by 
## makeCacheMatrix function above. 
## If the inverse has already been calculated and the matrix has not changed
## then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## get the cached value
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ## 
  y <- x$getMetrix()
  inv <- solve(y, ...)
  x$setInverse(inv)
  inv
}