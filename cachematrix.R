## R programming: Assignment 2
## Create a function (and environment) that can be used to either
##   1. calculate the inverse of a matrix or
##   2. retrieve the cached value of the inverse if it was already calculated



## makeCacheMatrix() is a function that creates an environment, containing:
##     * x = a matrix (assumed invertible)
##     * I = the matrix's inverse
##     * get()= function that sets the value of x
##     * set()= function that gets the value of x
##     * setinv() = function that sets the value of I
##     * getinv() = function that gets the value of I
## makeCacheMatrix() returns a list of the 4 above functions


makeCacheMatrix <- function(x = matrix()) {

  I <- NULL #I initialized as object within the makeCacheMatrix() envir to be used by later code 
  
  # set() function
  # does the same thing as first two lines in the main function: setvalue of x,  NULL value of I
  # purpose: when x reset, the value of I is cleared, forcing subsequent calls to cacheSolve() to recalculate the inverse
  set <- function(y) { 
    x <<- y     # 1. Assigns the input argument to the x object in the parent environment
    I <<- NULL  # 2. Assign NULL to the I object in the parent environment--clears any value of I cached by a prior execution of cacheSolve().
  }
  
  # get() function
  # getter for the matrix, x
  # Since the symbol x is not defined within get(), x retrieved from the parent environment of makeCacheMatrix().
  get <- function() x 
  
  # setinv() function
  # the setter for the inverse, I
  # uses <<- to assign the input argument to the value of I in the parent environment.
  setinv <- function(inv) I <<- inv
  
  # getinv() function
  # getter for the inverse, I
  # Since the symbol m is not defined within get(), m retrieved from the parent environment of makeCacheMatrix(). 
  getinv <- function() I 
  
  #assigns each of these functions as an element within a list(), and returns it to the parent environment.
  list(set = set, get = get, setinv = setinv,getinv = getinv) 
  
  #When function ends, returns fully formed object of type makeCacheMatrix() to be used by downstream R code  
}



## cacheSolve() REQUIRES an input argument of type makeCacheMatrix().
##  It computes the inverse of x returned by makeCacheMatrix above. 
##  If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
##  retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  #First, it calls the getinv() function on the input object.
  #this pulls up the current (cached) value of I in the parent environment 
  I <- x$getinv() 
  
  #Then it checks to see whether the result of  getinv() is NULL
  
  # if getinv() isn't NULL, we have a valid cached inverse and can return it to the parent environment
  if(!is.null(I)) {
    message("getting cached data") 
    return(I)
  }
  
  # if getinv() IS NULL, we calculate the inverse of the matrix   
  data <- x$get()#assigns data the get function, which is just the matrix in the parent environment
  I <- solve(data, ...)#assigns I the inverse of data, which is just the inverse of the matrix x in the parent environment
  x$setinv(I) # I is now set to the inverse of the matrix x IN THE PARENT ENVIRONMENT (overwrites that NULL)
  I # returns the inverse
}
