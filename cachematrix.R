makeCacheMatrix<-function(x=matrix()){
  ## this creates a special "matrix object that can cache its inverse
 
   makeCacheMatrix<-function(x=matrix())
     #define the argument with default mode of "matrix"
    inv<-NULL
     #initialize inv as NULL; to hole value of matrix
    set<-function(y) {
      #define a new function to assign new values in parent environment
      #If there is a new matrix, reset inv to NULL
      X<<-y
      inv<<-NULL
    }
    get<-function()x
    #returns the value of the matrix argument
    
    setinverse<-function(inverse) inv<<-inverse
    #assigns value of inv in parent environment
    
    getinverse<-function() inv
    #gets the value of inv where called
    
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
    # to be able to use the functions with the $
}

cacheSolve<-function(x,...){
  #returns a matrix that is the inverse of 'x'
  
  inv<-x$getinverse()
  
  if(!is.null(inv)) {
    message("getting cache data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data,...)
  
  x$setinverse(inv)
  inv
}