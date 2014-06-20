## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  #s - inverse matrix 
  s <- NULL # default value = NULL
  
  #set function: x and reset s
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  
  # get function: get x
  get <- function() x
  
  # setinverse function: set inverse matrix
  setinverse <- function(solve) s <<- solve
  
  # getinverse function: get inverse matrix
  getinverse <- function() s
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  #get s
  s <- x$getinverse()
  
  #check if it is not null
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  
  #if it is null, calculate the inverse
  data <- x$get()
  s <- solve(data, ...)
  x$setinverse(s)
  s
}
