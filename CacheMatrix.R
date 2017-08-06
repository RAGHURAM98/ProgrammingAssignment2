## Programming Assignment 2 - R Programming


## makeCacheMatrix:
## To facilitate this caching, you first create a special
## matrix that will help us with this by using the
## makeCacheMatrix function.  The input into this function
## is simply a variable of type matrix.
##

makeCacheMatrix <- function(x = matrix()) {
  
  # Initially set to NULL
  # Changes when the user sets the value
  inv <- NULL
  
  # set function
  # Sets the matrix itself but not the inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # get function
  # Gets the matrix itself but not the inverse
  get <- function() x
  
  # Manually set the inverse
  setinverse <- function(inverse) inv <<- inverse
  
  # Get the inverse
  getinverse <- function() inv
  
  # Encapsulate into a list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)	
}

## cacheSolve:
## Once you create this matrix, you use the cacheSolve
## function to compute the inverse and cache the result


cacheSolve <- function(x, ...) {

  inv <- x$getinverse()
  
  # If it has...
  if(!is.null(inv)) {
    # Simply return the computed inverse		
    message("Getting cached matrix")
    return(inv)
  }
  
  # If it hasn't...
  # Get the matrix itself
  data <- x$get()
  
  # Find the inverse
  inv <- solve(data, ...)
  
  # Cache this result in the object
  x$setinverse(inv)
  
  # Return this new result
  inv    
}

#x <- matrix(5:8, nrow=2, ncol=2)
#m <- makeCacheMatrix(x)
#s <- cacheSolve(m)
#print(s)
