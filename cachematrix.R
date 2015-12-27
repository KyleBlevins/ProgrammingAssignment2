##makeCacheMatrix creates a matrix to cache x
makeCacheMatrix <- function(x = matrix()) {
  nm <- NULL
  set <-function(y) {
    x <<- y
    nm <<- NULL
  } 
  get <-function() x
  setmatrix <- function(solve) 
    nm <<- solve
  getmatrix<-function() nm
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}
##cacheSolve returns a matrix that is the inverse of x
cacheSolve <- function(x = matrix(), ...) {
  nm <-x $getmatrix()
  if(!is.null(nm)){
    message("getting cached data")
    return(nm)
    ##Get cached data and return it
  }
  matrix <- x$get()
  nm <- solve(matrix, ...)
  x$setmatrix(nm)
  nm
}
