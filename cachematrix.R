
## Introduction
## Matrix inversion is usually a costly computation 
## and there are some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly . 
## The pair of functions (makecacheMatrix and cacheSolve) are used to 
## cache the inverse of a matrix.



## function: makeCacheMatrix 
## creates a special "matrix object",
## which is really a list containing a function to
## 1. set the value of the Matrix
## 2. get the value of the Matrix
## 3. get the value of the Inverted Matrix
## 4. get the value of the Inverted Matrix

makeCacheMatrix <- function(x = matrix()) 
{
  
  myMatrix <- NULL
  
  set <- function(y) 
  {
    x <<- y
    myMatrix <<- NULL
  }
  get <- function() x
  
  setInverse <- function(inverseMe) myMatrix <<- inverseMe
  getInverse <- function() myMatrix
  
  list(
        set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse
      )
  
}



## function: cacheSolve 
## computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated 
## (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'
  
  myMatrix <- x$getInverse()
  
  if(!is.null(myMatrix))
  {
    message("getting cached data")
    return(myMatrix)
  }
  
  data <- x$get()
  myMatrix <- solve(data, ...)
  x$setInverse(myMatrix)
  myMatrix

}
