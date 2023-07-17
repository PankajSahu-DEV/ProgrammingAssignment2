
makeCacheMatrix <- function(x) {

  cache <- NULL
  
  
  set <- function(matrix) {
    x <<- matrix 
    cache$inverse <<- NULL 
  }
  

  get <- function() {
    x  
  }
  
  
  getInverse <- function() {
    
    if (!is.null(cache$inverse)) {
      message("Getting cached inverse")
      return(cache$inverse)
    }
    
    
    message("Calculating inverse")
    cache$inverse <- solve(x)
    cache$inverse
  }
  

  list(set = set, get = get, getInverse = getInverse)
}


cacheSolve <- function(x) {

  if (!is.function(x$getInverse)) {
    stop("Input is not a valid cache matrix object.")
  }
  
 
  x$getInverse()
}
