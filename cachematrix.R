## Put comments here that give an overall description of what your
## functions do

## Caching the matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse<-NULL
  set<-function(y){
    x<<-y
    inverse<<-NULL
  }
  get<-function() x
  setinverse<-function(inv)inverse<<-inv
  getinverse<-function() inverse
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Matrix Inversion

cacheSolve <- function(x, ...) {
  inverse<-x$getinverse()
  if(!is.null(inverse)){
    message("getting data from cache")
    return(inverse)
  }
  data<-x$get()
  inverse<-solve(x,...)
  x$setinverse(inverse)
  inverse
}
