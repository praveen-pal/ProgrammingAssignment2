############# 
#this pair of functions compute the inverse of matrix ,
# this function cache the inverse of a matrix beause of costly and time consuming computation. 
##########

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m<-null
  set<-function(y){  # set the value of matrix
      x<<-y
      m<<-null
  }
  get<-function() x  # get the value of matrix
  
  setinverse<-function(solve) m<<-solve # set the inverse of matrix
  getinverse<-function() m              # get the inverse of matrix
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve: This function computes and returns the inverse of the special "matrix" returned by makeCacheMatrix 
## Return the inverse of matrix 'x'
cacheSolve <- function(x, ...) {
         
  m<-x$getinverse()
  #If the inverse has already been calculated then retrieve the inverse from the cache.
  if(!is.null(m)){
      message("getting data from cache")
      return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}
