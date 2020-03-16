## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  #initialising variable inverse to store the inverse of the matrix x
  inverse <- NULL
  #Creating a function set to set the value of the matrix
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  #get is a function that returns the original matrix
  get <- function() x
  #setInverse can be called to set the inverse of the matrix x
  setInverse <- function(m) inverse <<- m
  #getInverse will return the inverse matrix of x stored in variable x
  getInverse <- function() inverse
  
  #returning the list of all the functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  #calling get inverse
  m <- x$getInverse()
  
  #if the the getInverse function doesnot retuns null then will retun the inverse
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  #if the above if statement condition calculates to false
  #get the original matrix
  data <- x$get()
  #calcu;late the inverse of the matrix
  m <- solve(data)
  #set the calculated  inverse of the matrix to the inverse variabl 
  x$setInverse(m)
  m
  
  
}

