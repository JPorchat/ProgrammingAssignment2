## These two functions (makeCacheMatrix and cacheSolve) caches the inverse of
## a matrix so futher calculations can happens faster

## makeCacheMatrix creates a matrix object in order to prepare the envoiromnent
## to invert the matrix and keep it available to generate the inverted matrix

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverted matrix
## 4. get the value of the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  
  ## Clean the matrix with NULL
  invmat <- NULL
  set <- function(y) {
    x <<- y
    invmat <<- NULL
  }
  
  ## Loads the values to the matrix
  
  get <- function() x
  setinverse <- function(inverse) invmat <<- inverse
  getinverse <- function() invmat
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve gets the cached output from makeCacheMatrix and inverts it using
## the R solve functon solve(X). This function assumes that X is invertible.

cacheSolve <- function(x, ...) {
  
  invmat <- x$getinverse()
  
  ## Verifies if the inverted matrix is already cached. If so, returns the cached
  ## inverted matrix.
  if(!is.null(invmat)) {
    return(invmat)
  }
  
  ## If the inverted matrix is not cached, invert it using solve() function
  values <- x$get()
  invmat <- solve(values)
  x$setinverse(invmat)
  
  ## Return a matrix that is the inverse of 'x'  
  invmat
  
}
