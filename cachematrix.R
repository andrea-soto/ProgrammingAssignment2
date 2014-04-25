## The following functions calculate the inverse of a matrix.
## 'makeChacheMatrix' is used to create a list of functions that perform the following tasks: 
## a) Create a matrix b) See or get the matrix c) Save in Inverse and d) Retreive the Inverse from cache
## c) and d) are used used by 'cacheSolve' to save the Inverse when it is calculated and retreive instead of re-calculating it

## The 'makeCacheMatrix' returns four functions in the form of a list.
makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     
     get <- function() x
     
     setInv <- function(inverse) inv <<- inverse
     
     getInv <- function() inv
     
     list(set = set, get = get,
          setInv = setInv,
          getInv = getInv)
}

## The 'cacheSolve' returns the inverse of a matrix.
## If the inverse has not been computed before, the function will calculate it and save it in cache
## before returning the result. Otherwise, the inverse is read from cache and returned witout computation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

     inv <- x$getInv()
     if(!is.null(inv)) {
          message("Getting cached data")
          return(inv)
     }
     
     data <- x$get()
     inv <- solve(data, ...)
     x$setInv(inv)
     inv
     
}
