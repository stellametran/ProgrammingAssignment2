## The function creates a special matrix that can keep cache of the matrices 
## data and its inverse


## makeCacheMatrix creates a special matrix that do the following:
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverseMatrix
## 4.  get the value of the inverseMatrix

makeCacheMatrix <- function(x = matrix()) {
  
  inverseMatrix <- NULL
  
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inv) inverseMatrix <<- inv
  getinverse <- function() inverseMatrix
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## The following function calculates the inverse of the special matrix created at 
## makeCacheMatrix.If the inverseMatrix has already been calculated it sets cached inverse
## otherwise, it calculates the inverse of the matrix and sets the value of 
## the inverseMatrix in the cache via the `setinverse` function.

cacheSolve <- function(x, ...) {
 
  inverseMatrix <- x$getinverse()
  
  if(!is.null(inverseMatrix)) {
    message("getting cached data - 2nd run on")
    return(inverseMatrix)
  }
  
  message("calculating inverse matrix - 1st run")
  data <- x$get()
  inverseMatrix <- solve(data)
  x$setinverse(inverseMatrix)
  inverseMatrix
}
