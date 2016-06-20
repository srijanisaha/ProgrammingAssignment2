## Program: Caching the Inverse of a Matrix
## This program deals with the tedious calculation of the
## inverse of a matrix. Because the calculation can be costly,
## the program instead caches the value of the inverse so that
## the inverse is not repeatedly found. This is advantageous
## when the values of the matrix has not changed and so the 
## inverse is the same (and does not need to be recalculated)


##The function makeCacheMatrix creates a "matrix" through a list
## with several functions to set and get the value of the matrix
## as well as set and get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
      inv = NULL
      set <- function (y)
      {
              x <<- y
              inverse <<- NULL
      }
      get <- function ()
              x
      setInverse <- function (inverse) inv <<- inverse
      getInverse <- function() inv
      list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve calculates the inverse of the "matrix" after checking if the
## inverse is calculated. If the inverse is already calculated, then it prints the 
## message of "getting cached data" and does not calculate the inverse. If there
## is no inverse already calculated, the function calculates the inverse and caches the data

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv))
        {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
