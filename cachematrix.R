## Put comments here that give an overall description of what your
## functions do
## Coursera Data Science: R Programming 
## Week 3 Assignment starting January 9, 2019; 
  
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
## This function creates a special "matrix" object that can cache its inverse
  
makeCacheMatrix <- function(x = matrix()) { ## define default mode of "matrix"
    inv <- NULL                             ## initializing inv as NULL which will hold value of matrix inverse 
    set <- function(y) {                    ## define the set function to assign new 
        x <<- y                             ## value of matrix in parent environment
        inv <<- NULL                        ## if there is a new matrix, reset inv to NULL
    }
    get <- function() x                     ## defining the get fucntion whic will return value of the matrix argument
    
    setinverse <- function(inverse) inv <<- inverse  ## assigns value of inv in parent environment
    getinverse <- function() inv                     ## gets the value of inv where called
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  

  }
  
  
 ## Write a short comment describing this function
 ## Function to compute the inverse of the matrix returned by makeCacheMatrix.
 ## cacheSolve will retrieve the inverse from the cache
  
  cacheSolve <- function(x, ...) {
     inv <- x$getinverse()
     if(!is.null(inv)) {
         message("getting cached data")
         return(inv)
     }
     data <- x$get()
     inv <- solve(data, ...)
     x$setinverse(inv)
     inv
  }