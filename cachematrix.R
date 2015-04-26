## The purpose of these functions are to 
## cache computations that may be time-consuming

## This function creates a matrix that can cache the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL

}
get <- function() x
setinverse <- function(inverse) inv <<- inverse
getinverse <- function() inv
list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The following function calculates the inverse of the matrix. 
##First, the function checks to see if the inverse has already been calculated. 
##Second, if the inverse has been caluclated,it gets the inverse from the cache 
##and skips the computation.Otherwise, it calculates the inverse of the 
##data and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("Retreiving Cached Data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
