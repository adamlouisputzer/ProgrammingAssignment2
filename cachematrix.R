## Coursera programming assingment 2
#make functions cachesolve and makecachmatrix so that they 
#can find the inverse of a matrix and also cache this result
#if the matrix does not change

## This function can create and matrix and have its result
#be invertable with a cache storing the inverse from calling
#the cache solve method
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inv_in) inv <<- inv_in
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#this function returns the inverse of a matrix if its already computed
#or it will compute the inverse and then store the result
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}