# These functions can be used to create a special matrix that caches it's
# inverse if it is solved using "cacheSolve" function

## Write a short comment describing this function

# makeCacheMatrix function creates a "matrix" that is list of four functions.
# get returns the matrix, set is used to store a matrix and it also replaces 
# cached inverse to NULL. setinvmatrix is used to set the inverse and 
# getinvmatrix returns the inverse

makeCacheMatrix <- function(x=matrix()){
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinvmatrix <- function(invmatrix) m <<- invmatrix
  getinvmatrix <- function() m
  list(set = set, 
       get = get,
       setinvmatrix = setinvmatrix,
       getinvmatrix = getinvmatrix)
}


# cacheSolve function checks if x returns non-NULL value with its x$getinvmatrix
# i.e. if the matrix has inverse cached. If inverse is cached, it returns the 
# inverse. If it is not cached, it solves the  inverse and sets caches the inverse

cacheSolve <- function(x, ...) {
  m <- x$getinvmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  message("solving the inverse of the matrix")
  data <- x$get()
  m <- solve(data, ...)
  x$setinvmatrix(m)
  m
}
