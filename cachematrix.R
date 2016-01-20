## The two function here allow the user to:
## 1.Retrieve the inverse of a matrix if the inverse already exists
## 2. COmpute and return the inverse of the matrix if inverse does not exist


## This function creates a special "matrix" object that can cache its inverse.
## Assumption: The matrix supplied is always invertible.
## This function returns a list with each element being a function call
## Elements of the list returned:
## set - sets/modifies value of the matrix
## get - retrieves the matrix
## setinv - calculates inverse of the matrix
## getinv - retrieves inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(inverse) inv <<- inverse
      getinv <- function() inv
      list(set = set, get = get,setinv = setinv,getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inv <- x$getinv()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data)
      x$setinv(inv)
      inv
}