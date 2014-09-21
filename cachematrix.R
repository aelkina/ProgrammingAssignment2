##Creating a matrix that can cache its inverse
##Matrix x with inverse as inverse

makeCacheMatrix <- function(x = matrix()) {
##set the value of the matrix
##get the value of the matrix
## set the value of the inv.
## get the value of the inv.
  inverse=NULL
  set=function(y){
    x<<-y
  inverse<<-NULL  
}
get=function()x
setinv=function(inverseM) inv<<-inverseM
getinv=function() inverse
list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## Function creates inverse from matrix object created by the makecachematrix function
##checks if inverse already created, and returns it, if not creates the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse = x$getinv()
  if (!is.null(inverse)){
 
    message("Getting data from cache")
    return(inverse)
  }
  
  mat.data = x$get()
  inverse = solve(mat.data, ...)
  
  x$setinv(inverse)
  
  return(inverse)
}
