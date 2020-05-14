## These functions will create a cache of an inverse of a matrix in order to save on calculations. 
# Calculating a matrix inverse is computationally demanding and therefore, if this calculation needs 
# to be completed repeatedly, it is better to store the solution for a given matrix in memory 
# rather than calculate the value from scratch every time 

# first function, makeCacheMatrix, will create two objects: x (matrix) and i (inverse), and four 
#functions - 'getters' and 'setters' for x and i. 'Getters' will retrieve the value of the object
# that is stored in the parent environment, and 'setters' will set the value in the parent environment
# to the new value inputted 
# finally, creating  a list with the functions at the end will allow to retrieve them from outside
# the function 

makeCacheMatrix <- function(x = matrix()) {
  # initialises the value of the inverse to NULL 
  i <- NULL
  
  #setter function for x - the original matrix. If a new matrix is provided, y, that differs from x
  #then the value of x in the parent environment is overwritten with the new value and the value of 
  #inverse is reset to null 
  setx <- function (y) {
    x <<- y
    i <<- NULL
  }
  
  #getter function - retrieves value of x matrix 
  getx <- function () x 
  # setter function for the inverse - sets the value of i in the parent environment to the new value
  #stored in the variable 'inverse'
  seti <- function(inverse) i <<- inverse
  
  # getter function for i 
  geti <- function () i
  # returns the functions to the parent environment as elements of a list
  # (remember, last thing a function does gets returned
  # and creates explicit names for the functions so that they can be called by the expression x$... 
  # later on 
  list (setx = setx, getx = getx, seti = seti, geti = geti)
}




## This is the cacheSolve function, which will be able to calculate the inverse of the Cache Matrix object. 

cacheSolve <- function(x, ...) {
  i <- x$geti()
  
  # this part checks whether a cached value already exist to save the calculation - 
  # if not, it will reset the value of i to null 
  if(!is.null(i)) {
    message("getting cached matrix inverse")
    return(i)
  }
  new_matrix_value <- x$getx()
  i <- solve(new_matrix_value, ...)
  x$seti(i)
  i
  }


