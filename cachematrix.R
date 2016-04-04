## makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse.

## cacheSolve: This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
## Computing the inverse of a square matrix can be done with the solve function

## makeCacheMatrix creates a special "vector", which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of matrix
## 4. get the value of inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  m<- NULL
  
  set<- function(y){
    x <<- y
    m <<- NULL
  }
  
  get<- function() x
  
  setinv<- function(inv) m <<- inv
  getinv<- function() m
  
  list(set=set, get=get, setinv=setinv, getinv = getinv)
  
}


## cacheSolve calculates the inverse of the matrix created above. It first check if the inverse of 
## matrix has already been calculated. If so, return the inverse from the cache. Otherwise, calculate
## the inverse of matrix by using Solve function and set the value of the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
    m<- x$getinv()
    if(!is.null(m)){
      return(m)
    }
    
    data<- x$get()
    m<- solve(data)
    x$setinv(m)
    m

}


