## These two functions receive a matrix input (assumed as invertible) and return
## an inverse. during executing these functions, the inverse is calculated if the
## cache is empty, stored in cache and result returned. If input atrix hasn't
## changed then the matrix inverse is returned from cache 

## this function creates a list that has values required to be fed into the
## cacheSolve function (which caches the inverse for subsequent retrieval)
## the elements of the output are as follows; set= sets the initial values of
## the matrix and the inverse (which is set to NULL to ensure that if matrix changes
## old inverse is not output; get = gets the matrix needed to be inversed; 
## setinv = sets the value of inverse; getinv = gets the value of inverse)

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y)  {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) {i <<- inverse}
  getinv <- function() {i}
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function creates the inverse of the matrix input in earlier function.
## This function checks if the inverse cache is null, if null then calculates 
## inverse and stores it, else it retrieves inverse from cache and displays result

cacheSolve <- function(x,...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message ("getting cached data")
    return (i)
  }
  mtx <- x$get()
  i <- solve(mtx,...)
  x$setinv(i)
  i
}

