## Put comments here that give an overall description of what your
## functions do


## Make Cache of this function
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  # the function that set the matrix value
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # the function that retrieve the matrix value
  get <- function() x
  
  # set the value of matrix inverse
  setInv <- function(solvedInvValue) m <<- solvedInvValue
  
  # read the value of matrix inverse from cache
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

## Solve the given matrix. If a solution has been calcualted and cached,
## read if from the existing cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # check if the inverse of x has already existed
  m <- x$getInv()
  # if it exists, read it from the cache
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # if the solution has not been calcualted,
  # read it and calcualte its inverse
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  m
  
}

