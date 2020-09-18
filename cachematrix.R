#These functions create a cached matrix, and keep the inverse of that matrix
#even though finding the inverse matrix seems like a short process,
#it can take longer for larger matrices, these functions
#keep the cache of the inverse, so the second time you need the inverse
#just call the cached inverse


makeCacheMatrix <- function(x = matrix()) {
        ##Creates a matrix, returns a list of functions
        ##you can set and get the matrix
        ##you can also getinverse to see the cached inverse
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(invmatrix){
    inverse<<-invmatrix
  }
  getinverse <- function() inverse
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}



cacheSolve <- function(x, ...) {
        ## If calculated before, Return a matrix that is the inverse of 'x'
        ## If not, calculate it and save it for later use
        ## Assuming the supplied matrix is invertible
  
  inverse <- x$getinverse()
  if (!is.null(inverse)){
    message("getting cached inverse")
    return(inverse)
  }
  data = x$get()
  inverse <- solve(data,...)
  x$setinverse(inverse)
  inverse
}