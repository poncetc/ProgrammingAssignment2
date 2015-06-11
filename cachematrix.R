## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse<-NULL #restores to null the value of the inverse matrix
  set<-function(y) {
    x <<- y #substitutes the matrix x with y (the input) in the main function (makeCacheMatrix)
    inverse <<- NULL #restores to null the value of the inverse matrix
  }
get<- function() x #get is a function that returns the matrix x stored in the main function. 
setsolve<- function(solve) inverse<<-solve
getsolve<-function() inverse #setsolve and getsolve are functions very similar to set and get.
#They don't calculate the inverse matrix, they simply store the value of the input
#in a variable inverse into the main function makeCacheMatrix (setsolve) and return it (getsolve)
#To store the 4 functions in the function makeCacheMatrix, we need the function list(),
# so that when we assign makeCacheMatrix to an object, the object has all the 4 functions.
list(set = set, get = get,
     setsolve = setsolve,
     getsolve = getsolve)
}

# Input of cacheSolve is the object where makeCacheMatrix is stored.

cacheSolve <- function(x, ...) {        
  ## Returns a matrix that is the inverse of 'x'
  inverse <- x$getsolve() #verifies that the value inverse, stored previously with getsolve,
  # exists and is not NULL
  if(!is.null(inverse)) { 
    message("getting cached data")
    return(inverse) #
  } 
  data <- x$get()
  inverse <- solve(data, ...)
  x$setsolve(inverse)
  inverse
}

