# These functions can be used to create a special matrix that caches it's
# inverse if it is solved using "cacheSolve" function

## Write a short comment describing this function

# makeCacheMatrix function creates a "matrix" that is list of four functions.
# get returns the matrix, set is used to store a matrix and it also replaces 
# cached inverse to NULL. setinvmatrix is used to set the inverse and 
# getinvmatrix returns the inverse.
# if no input matrix is given, the function creates an empty matrix


makeCacheMatrix <- function(x=matrix()){
  m <- NULL # the cache value is initialized to NULL
  set <- function(y) {
    x <<- y # New value is set to the x
    m <<- NULL # If matrix gets new value, the cache is returned to NULL
  }
  get <- function() x # Set returns the matrix
  setinvmatrix <- function(invmatrix) m <<- invmatrix # setinvmatrix sets the cache
  getinvmatrix <- function() m # getinvmatrix returns the cache
  list(set = set, 
       get = get,
       setinvmatrix = setinvmatrix,
       getinvmatrix = getinvmatrix)
}


# cacheSolve function checks if x returns non-NULL value with its x$getinvmatrix
# i.e. if the matrix has inverse cached. If inverse is cached, it returns the 
# inverse. If it is not cached, it solves the  inverse and sets caches the inverse

cacheSolve <- function(x, ...) {
  m <- x$getinvmatrix() #getinvmatrix function of input matrix is called
                        # if cache hasn't been solved, it returns NULL
  if(!is.null(m)) { 
    message("getting cached data")
    return(m) # if cache is not NULL, it will be returned
  }
  # If cache is NULL, the inverse is solved, value cached and returnes
  message("solving the inverse of the matrix")
  data <- x$get() # get the data matrix
  m <- solve(data, ...) # solve the inverse of the matrix
  x$setinvmatrix(m) # Set the cache value
  m # Return inverse
}
