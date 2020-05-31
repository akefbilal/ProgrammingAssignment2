## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  #input x is set as a matrix
  invrS <- NULL                                         #solved value is set a NULL first
  set <- function(y){                         
    x <<- y                                             #x and invrS variables are set in the Global environment
    invrS <<- NULL
  }
  get <- function() {x}                                 #method to get the matrix
  setInverse <- function(inverse) {invrS <<- inverse}   #sets the inverse of matrix
  getInverse <- function() {invrS}                      # gets the inverse of matrix
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}

## Write a short comment describing this function


#This function computes the inverse of the matrix created by the above function 
#If the matrix has not changed and the inverse is already calculated it retrive the 
#cached inverse 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invrS <- x$getInverse()                         #if inverse is already calculated 
  if(!is.null(invrS)){
    message("getting cached data")
    return(invrS)                                 #return the cached data
  }
  mat <- x$get()
  invrS<- solve(mat, ...)                         #if inverse is not cached then solve it mathematically and
  x$setInverse(invrS)                             #gives the inverse of matrix
  invrS

}

