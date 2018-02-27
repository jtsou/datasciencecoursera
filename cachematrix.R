#Matrix inversion is usually a costly computation and there may be some benefit to 
#caching the inverse of a matrix rather than compute it repeatedly 
#(there are also alternatives to matrix inversion that we will not discuss here). 
#Your assignment is to write a pair of functions that cache the inverse of a matrix.

#Write the following functions:

#  makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


#Computing the inverse of a square matrix can be done with the solve function in R. 
#For example, if X is a square invertible matrix, then solve(X) returns its inverse.

#For this assignment, assume that the matrix supplied is always invertible.





makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}



cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}


## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#this function cache the inverse of a Matrix
#1. Set the value of vector
#2. Get the value of vector
#3. Set the value of inverse of matrix
#4. Get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL   
  set <- function(y) { #2
    x <<- y
    inv <<- NULL
    
  }
  get <- function() x #1
  setinverse <- function(inverse) inv <<- inverse #3
  getinverse <- function() inv #4
  list(set=set, get=get, setinverse=setinverse,getinverse=getinverse)
}


## Write a short comment describing this function

#This function returns the inverse of matrix,
#it checks if the inverse has already been returned first. If so, it gets the result.
#if not, it retrieve the matrix from cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse{
    if(!is.null(inv)){
      message('getting cached data')
      return (inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
  }
  
}