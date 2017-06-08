## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) { ## define the arguments of the function and call it "makeCacheMatrix"
  inv <- NULL  #introduce the "inv" variable and sets it as NULL; this variable will include values of matrix inverse 
    x <<- y  ## assign a value to the matrix itself in a parent an environment (different from the current environment)
  inv <<- NULL  ## set values of inv (matrix inverse) in the parent environment to NULL
                                          }
get <- function() x    ## define the "get" function - returns value of the matrix itself 
{
  setinverse <- function(inverse) inv <<- inverse  ## defines the arguments of setinverse; this function will assign value of inv in parent environment
  getinverse <- function() inv                     ## gets the value of inv (in other words, it gets the matrix inverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache


## Write a short comment describing this function

cacheSolve <- function(x, ...) { ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse() #get the current state of the inverse matrix, check if it has been computed
  if(!is.null(inv)) { #if it has...
    message("getting cached data")#display this message
    return(inv)#and return the value of the inverse matrix 
  }
  data <- x$get()#if it has not
  inv <- solve(data, ...)#retrieve the matrix itself
  x$setinverse(inv) #find the values of the inverse matrix
  inv #show the new result
}
