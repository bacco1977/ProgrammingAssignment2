## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##creates a vector with 4 functions : set and get for the matrix 
##and set and get for the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  #set function to set the matrix initialized with a NULL inverse
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  #get function that returns the matrix
  get <- function() x
  #sets the inverse of the matrix
  setinv <- function(inv) i <<- inv
  #returns the inverse of the matrix
  getinv <- function() i
  #returning the vector with the 4 functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## Write a short comment describing this function
##Calculates the inverse of a matrix reading the cache first
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ##reads from the cache
  i <- x$getinv()
  ##checks the cache
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  ##if not cached, calculates it and sets the value in the cache
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}

