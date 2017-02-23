## The makeCacheMatrix and cacheSolve functions take a square matrix and calculates its inverse. Since the Solve function of
## calculating the inverse of a matrix may be memory-intensive, the program makeCacheMatrix stores the inverse calculated by cacheSolve
## such that, if the same matrix is provided for inverse caculation, the program searches the cache to find if the same calculation
## has been done previously. If the inverse is in the cache, the function returns the solution stored in cache (and notifying the
## user of the retrieval from cache). In case the cache does not contain the inverse, the cacheSolve function calculates and returns 
## the inverse, while storing the new inverse in cache.

## This function actually contains a list of functions to set a square matrix, display the matrix, setting the inverse of the matrix
## and finally displaying the matrix.

makeCacheMatrix<-function(x = matrix()){
  I<-NULL
  set <- function(y){
    x<<-y
    I<<-NULL
  }
  get<-function() x
  setInverse<-function(solve) I <<-solve
  getInverse<-function() I
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function first checks to see if the inverse of the matrix provided has been calculated before and retrieve it if has been 
## calculated. If not, the function calculates the inverse and stores it in the cache.

cacheSolve<-function(x, ...){
  I<-x$getInverse()
  if(!is.null(I)){
    message("getting cached data")
    return(I)
  }
  data<-x$get()
  I<-solve(data, ...)
  x$setInverse(I)
  I
}
