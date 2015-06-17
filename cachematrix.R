## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  ##First we make sure the value of m is NULL before storing a new matrix on the cache
  m <- NULL 
  ##Store data of the new matrix on the cache
  set <- function() {
    x <<- y
    m <<- NULL
  }
  ##Recovers the value of the matrix 
  get <- function() x
  ##Sets the value of the inverse
  setinv <- function(solve) m <<- solve
  ##Shows the sotored result of the inverse
  getinv <- function() m
  ##Creates a list with all the cached values
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}


## This function computes the inverse of the special "matrix" returned by
## `makeCacheMatrix` above. If the inverse has already been calculated 
## (and the matrix has not changed), then `cacheSolve` should retrieve the
## inverse from the cache 

cacheSolve <- function(x = matrix(), ...) {
  ## Return a matrix that is the inverse of 'x'
  ##First we call m to see if the inverse has been calculated before and 
  ##if its value is NULL we proceed to call the solve function and store the inverse on cache
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
