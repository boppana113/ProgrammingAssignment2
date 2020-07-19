## Filename: cachematrix.R
## Description: This file contains a several functions for working with invertibale matrices
## Author: Sriram Boppana
## Email: boppana113@gmail.com

# makeVector allows users to (1) set the value of the vector (2) get the value of the vector (3) set the value of the mean
# (4) get the value of the mean
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
# The following function calculates the mean of the special "vector" 
# created with the above function but first checks if the mean has already been calculated
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

makeCacheMatrix <- function(x = matrix()) {

}


## Calculate the inverse of the "matrix" created with the above function
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  m <- x$get()
  i <- solve(m, ...)
  x$setinverse(i)
  i
}
