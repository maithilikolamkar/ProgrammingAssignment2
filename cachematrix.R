##Program to cache inverse of matix(calculated using solve) and retrieve cached values rather than recomputing


## Function name: makeCacheMatrix
## This function creates a list containing functions to perform following actions:
## 1) Set the value of the matrix
## 2) Get the value of the matrix
## 3) Set inverse of the matrix
## 4) Get inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function()
    x
  
  setInverse <- function(solve)
    m <<- solve
  getInverse <- function()
    m
  list(
    set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse
  )
}


## Function name: cacheSolve
## This function calculates the inverse of the matrix if it is not already computed and saves it in cache
## Else it returns the mean from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  ## Check if cached value exists
  if (!is.null(m)) {
    message("Getting cached data")
    return(m)
  }
  ## Compute if cache does not exist
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  return(m)
}
