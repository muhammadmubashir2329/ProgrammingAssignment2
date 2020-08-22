## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function will accept a matrix and return list of functions to cache or 
## retrieve inverse of matrix
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x<<- y
    i<<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i<<- inv
  getinverse <- function () i
  list(set =set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## This function will check if cacheMatrix has already computed inverse, to do
## it will first check cache and if computed return it, else compute inverse,
## save it in cache and return it.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)){
    print("getting cached data")
    return(i)
  }
  data <- x$get()
  i<- solve(data,...)
  x$setinverse(i)
  i
}
