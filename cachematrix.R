## makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse.

## Write a short comment describing this function

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


## Write a short comment describing this function

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


