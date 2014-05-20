## Programming Assignment #2
## Matrix inversion is usually a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than computing it repeatedly (there are also alternatives to 
## matrix inversion that we will not discuss here). Your assignment is to write a pair of 
## functions that cache the inverse of a matrix.



##  This function creates a special "matrix" object that can cache its inverse.
##  Example of how to use this function:
##  a <- makeCacheMatrix(matrix(1:4,2,2)) 


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  ##subfunctions defined below
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed), then 
## cacheSolve will retrieve the inverse from the cache.  Otherwise cacheSolve will calculate
## the inverse
## Example of how to use this function:
## cacheSolve(a)


cacheSolve <- function(x, ...) {
  m <- x$getinv()
  
  ##check for previously cached data
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ##otherwise use solve() to calculate the inverse 
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

## Usage of both functions, with example output
## > a <- makeCacheMatrix(matrix(1:4,2,2)) 
## > cacheSolve(a)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
