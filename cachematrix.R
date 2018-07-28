## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # initially making inverse of the matrix as NULL
  inverse<-NULL
  
  # Here set function is used to set the value of x
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  
  # get function returns the matrix x
  
  get<-function()x
  
  # setInverse() takes a matrix as an argument and set that argument to the inverse of the required matrix
  
  setInverse<-function(inv){
    inverse<-inv
  }
  
  # getInverse() returns the inverse of the required matrix
  
  getInverse<-function(){
    inverse
  }
  
  # returns the list of nested function with their names
  
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  #getting the inverse from the getInverse() in makeCacheMatrix()
  
  inverse<-x$getInverse()
  
  # check whether the inverse is already calculated 
  if(!is.null(inverse)){
    message("Getting cached data")
    
    # if it is calculated then return the inverse
    return(inverse)
  }
  
  # else get the data
  data<-x$get()
  
  # finding the inverse of the required matrix
  inverse<-solve(data,...)
  
  # setting the inverse of the required matrix....
  
  x$setInverse(inverse)
  
  #return the inverse of the matrix
  inverse
        ## Return a matrix that is the inverse of 'x'
}
