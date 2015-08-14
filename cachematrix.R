## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#The first function, "makeCacheMatrix" creates a special "vector", 
#which is really a list containing a function to:

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  #1. Set the value of the vector
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  #2. Get the value of the vector
  get <- function() x
  
  #3. Set the value of the mean
  setsolve <- function(solve) m <<- solve
  
  #4. Get the value of the mean
  getsolve <- function() m
  
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function
#The following function calculates the inverse of the special "vector" created with the above function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  #First checks to see if the inverse matrix has already been calculated and get the matrix from the cache
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  #Or calculates the inverse of the data and sets the value of the inverse in the cache via the setsolve function
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
