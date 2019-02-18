## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {   ## define arguement as "matrix"
  inv <- NULL                                 ##initialize inv as NULL for placeholder of matrix inverse 
  set <- function(y) {                        ##define set function
    x <<- y                                   ##value of matrix
    inv <<- NULL                              ##if new matrix, assign NULL to inv
  }
  get <- function() x                         ##define get function for returning the matrix
  setinv <- function(inverse) inv <<- inverse ##assigns inv value
  getinv <- function() inv                    ## gets the inv value
  list(set = set, get = get,                  ##need this for the $ operator in cacheSolve
       setinv = setinv,
       getinv = getinv)
}

## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- inverse(data, ...)
  x$setinv(inv)
  inv    ## Return a matrix that is the inverse of 'x'
}
