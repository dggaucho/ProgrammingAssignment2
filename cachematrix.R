## Diego Garcia Lopez
## Assigment 2- Cahcing the Inverse of a Matrix

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
## x is a square invertible matrix
## return a list containing functions to 
  ## 1 = set the matrix
  ## 2 = get the matrix
  ## 3 = set the inverse
  ## and 4 = get the inverse
  ## the list is used as a parameter to cachesolve below
  
  i = NULL
  set = function(y) { 
      x <<- y
      i <<- NULL
    }
  get = function() x
  setinverse = function(solve)  i<<- solve
  getinverse = function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
##  x is the output of makeCacheMatrix above
## return the inverse of original matrix 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i<-x$getinverse()
  ## test if the inverse has been calculated
  if(is.null(i)){
    #it has so get it from cache and avoid extra calculations  
    message("getting cached data")
      return(i)
  }
  # else calcualtes the inverse and returns it below
  data<-x$get()
  i<-solve(data, ...)
  x$setinverse(i)
  i
}
