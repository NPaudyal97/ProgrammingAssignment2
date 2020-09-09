## A pair of functions that cache the inverse of a matrix


## Make a special matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse){
    i <<- inverse  
  } 
  getinverse <- function() i
  list(set=set, get=get, 
       setinverse=setinverse,
       getinverse=getinverse)
}

## This function computes the inverse of the matrix computed
## by the function above
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)){
    message('getting cached data')
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
