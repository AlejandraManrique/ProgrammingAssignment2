## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix creates 4 functions for an square matrix 
## (set a matrix, get that matrix, set the inverse and get the inverse)
## argument of the matrix must be passed

makeCacheMatrix <- function(x = matrix()) {

  mRes <- NULL
  
  ## declatarion of function set
  set <- function(mInt) {
    x <<- mInt
    mRes<<-NULL
  } 
  
  ## declatarion of function get
  get <- function() x
  
  ## cálculo de la inversa
  setsolve <- function(solve) mRes <<- solve(x)
  
  ## obtencion de la matriz inversa de memoria
  getsolve <- function() mRes
  
  print(mRes)
  
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}


## Write a short comment describing this function
## calculates the inverse of a matrix (cached) if it has not been calculated before, 
## otherwise returns the inversed matrix from the cache

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  
  ## try to get the inversed matrix 
  mInt <- x$getsolve()
  
  ## if it's null it's because it has not been calculated before
  ## otherwise we just return it and function ends
  if(!is.null(mInt)) {
    message("getting cached data")
    return(mInt)
  }
  
  ## if it's 1st time calculated, we get the matrix
  matdata <- x$get()
  
  ## the inverse matrix is calculated
  mRes <- solve(matdata, ...)
  
  ## and its cached
  x$setsolve(mRes)
  
  ## and returned it
  mRes
  
  
}
