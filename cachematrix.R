## Coursera R Programming Course - Programming Assignment 2
## @author Bas Wenneker
## @email b.wenneker@gmail.com

## makeCacheMatrix - This function creates a special "matrix" object 
## that can cache its inverse.
##
## matrix x   Matrix object that's extended with a cache method.
makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() { x }
  setinverse <- function(inverse) { i <<- inverse }
  getinverse <- function() { i }
  
  list(
    set = set, 
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}

## cacheSolve -  This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), then 
## the cachesolve should retrieve the inverse from the cache.
##
## makeCacheMatrix(list) x   Extended matrix object that's 
##                            extended with a cache method to get the 
##                            inverse.
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

testCacheSolve <- function() {
  y<-matrix(c(1,2, 11,12), 2, 2)
  m<-makeCacheMatrix(y)
  inverse_m<-cacheSolve(m)
  if(identical(inverse_m, solve(y))){
    message("matrix inverted correctly")
    cacheSolve(m)
  }
}