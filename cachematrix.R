## Description:
## Below are a pair of functions, one that creates a special matrix that stores
## its inverse and can call it as well, while the other either computes the 
## inverse if it has not already been computed and stores it in the cache
## or retrieves it from te cache if it has been computed before

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL  ## instantiate inv with NULL value
  
  ## the funtion below sets the value of matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x ## this function retrives the matrix
  
  ## the function below sets the inverse of the matrix
  setinv <- function(solve) inv <<- solve 
  
  getinv <- function() inv ## this function retrives the inv of the matrix
  
  ## below is the lis that contains the previously defined funcitons
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {    
  
  ## retrive the inverse value from the special matrix generated above
  inv <- x$getinv()
  
  ## check whether the inverse already exists and return it if it does
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## if the inverse is null, then the rest of the code executes to compute it
  
  data <- x$get() ## retrive the data from the special matrix generated above
  
  inv <- solve(data, ...) ## computed the inverse of the matrix retreived above
  
  x$setinv(inv) ## set the inverse in the special matrix after computation
  
  inv ## Return a matrix that is the inverse of 'x'
}