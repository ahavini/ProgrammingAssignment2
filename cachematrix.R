## Put comments here that give an overall description of what your
## functions do
##
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # This function acts as a class, it creates a list that contains 4 member functions: set, get, setInv, and getInv. 
  # The <<- assignment operator is used so that the internal variables aren't seen by the outside environment. 
  
  xInv <- NULL # stores the inversion here 
  
  set <- function(y) {  # 
    x <<- y
    xInv <<- NULL # xInv is initalized to NULL  
  }
  # this function sets the square invertible matrix to the object created by the makeCacheMatrix(using_this_matrix) function
  get <- function() x # gets or returns the input matrix   
  setInv <- function(inv) xInv <<- inv # sets the inversed matrix   
  getInv <- function() xInv # returns the inversed matrix   

  list(set = set, get = get, setInv = setInv, getInv = getInv)
  # a list is returned that contains the functions:
  # x <- makeCacheMatrix(using_this_matrix)
  # x$set(newmatrix) # changes the matrix
  # x$get # gets the setted matrix
  # x$setInv # sets the inverted matrix
  # x$getInv # gets the inverted matrix
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          m <- x$getInv() # gets the inverted matrix from object x which will remain NULL (see line 6) if it's uncalculated  
  if(!is.null(m)) { # if statement works if inverted matrix (x$getInv) isn't NULL
    message("getting cached data")
    return(m) # return the calculated inversion
  }
  data <- x$get() ## if not calculated the use x$get to get the matrix object  
  m <- solve(data) # solve for the inverted matrix
  x$setInv(m) # set it to the object
  m # return m as the solved result

}
