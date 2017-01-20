## Programming Assignment 2 
## these two functions will cache the inverse of a matrix

## This first function will makeCacheMatrix creates a list containing a function to 
## 1. set / get the value of the matrix
## 2. set / get the inverse of the matrix


  makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) { 
      x <<- y
      m <<- NULL
    }
    get <- function() x
    set_Inverse <- function(solve) m <<- solve
    get_Inverse <- function() m 
    list(set = set, get = get, 
         set_Inverse = set_Inverse, 
         get_Inverse = get_Inverse)
  
  }


##The second fucntion cacheSolve() will return the inverse of the matrix. 
## if the inverse has already been computed cacheSolve() will print "getting cached data" 
  ## indicating the it is retrieving the value from cached info. 
  ##  if the inverse of the matrix has not been computed cacheSolve() will compute the inverse and store the value as cached data. 

cacheSolve <- function(x, ...) {
       m <- x$get_Inverse()
       if(!is.null(m)){ 
         message("getting cached data")
         return(m)
       }
       data <- x$get()
       m <- solve(data)
       x$set_Inverse(m)
       m
}
  

