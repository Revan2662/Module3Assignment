## There are two functions that work together to "cache" a provided matrix
## before either "solving" it using the solve() function or determining that
## the matrix had already been solved before and providing the previously 
## solved result.

## This function takes a provided matrix and "caches" it into "toCache". It also
## makes a variable "toInverse" that will be NULL. It also makes a number of
## simple functions: set, get, setInverse, and getInverse. These will be stored
## in a list format that can be specifically called later.

makeCacheMatrix <- function(x = matrix()) {
  toInverse <- NULL
  set <- function(toCache) {
    x <<- toCache
    toInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) toInverse <<- solve
  getInverse <- function() toInverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function pulls the previously provided matrix from the "makeCacheMatrix"
## and "solves" it to provide the inverse matrix with the solve() function. It
## checks if the toInverse variable has been changed, which it would've if this
## function was ran before, and simply returns the solved matrix again. It calls
## on the simple functions provided by "makeCacheMatrix" to interact with
## the provided matrix and returns the inverse of it as toInverse.

cacheSolve <- function(x) {
  toInverse <- x$getInverse()
  if(!is.null(toInverse)) {
    message("getting cached data")
    return(toInverse)
  }
  data <- x$get()
  toInverse <- solve(data)
  x$setInverse(toInverse)
  toInverse
        ## Return a matrix that is the inverse of 'x'
}