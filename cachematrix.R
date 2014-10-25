## Below are two functions that are used to create a special object that stores
## a numeric matrix and caches its inverse

## The first function creates a special "matrix", which is really a list 
## containing a function to 
## set the value of the matrix,
## get the value of the matrix,
## set the value of the inverse,
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv<-NULL
  
  set<-function (y){
    
    x<<-y
    
    inv<<-NULL
    
  }
  
  get<-function() x
  
  setinverse<-function(inverse) inv<<-inverse
  
  getinverse<-function() inv
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## It computes the inverse of the matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated and the matrix has not changed,
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  inv<-x$getinverse()
  
  if(!is.null(inv)){
    
    message("getting cached inverse")
    
    return(inv)
  }   
  
  ## Return a matrix that is the inverse of 'x'
  
  data<-x$get()
  
  if (nrow(data)!=ncol(data)) {
    
    message("matrix not squared")
    
    return()
  }
  
  inv<-solve(data,...)
  
  x$setinverse(inv)
  
  inv
}