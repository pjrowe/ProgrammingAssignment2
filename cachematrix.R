## These two functions are used to cache the inverse of a matrix, which is useful 
## because the calculation of the inverse can be time consuming.

## the makeCacheMatrix function creates a special "matrix" object. 
## The function takes a matrix as an argument and 
## returns a list of four functions.

makeCacheMatrix <- function(x = matrix()) {
     x_inv <- NULL
     set <- function(y) {
          x <<- y
          x_inv <<- NULL
     }

     get <- function() x
     setinverse <- function(x_inverse) x_inv <<- x_inverse
     getinverse <- function() x_inv
     list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve takes a special 'matrix' object (a list of four functions) 
## as an argument and returns the inverse of a matrix that was stored when 
## the special matrix object was created  (i.e., when the makeCacheMatrix was called
## with the matrix as an argument).  The cacheSolve function first checks   
## to see if the inverse has already been calculated.  If not, then the inverse
## calculation is done.  Otherwise, the stored inverse matrix is returned, saving
## calculation time and resources.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     x_inv <- x$getinverse() 
     if(!is.null(x_inv)) {
          message("getting cached data")
          return(x_inv)
     }
     data <- x$get()
     x_inv <- solve(data, ...)
     x$setinverse(x_inv)
     x_inv
     
}
