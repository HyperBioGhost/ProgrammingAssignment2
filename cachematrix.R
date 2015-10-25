## Following the example, makeCacheMatrix returns a list that contains functions
## which could set and read the inverse of a matrix to variable i
## And cacheSolve would actully call solve function to calculate the inverse
## then cache it by calling setinver function from makeCacheMatrix


## makeCacheMatrix creates by default a 2 by 2 square invertible matrix,
## and defines the functions set and read the inverse of the matrix.

makeCacheMatrix <- function(x = matrix(c(4,3,3,2), nrow = 2)) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setinver <- function(inverse) i <<- inverse
      getinver <- function() i
      list( set = set, 
            get = get,
            setinver = setinver,
            getinver = getinver
      ) ##To pile the functions in a list to be called by using $
      
}


## cacheSolve actually calculates the inverse of a matrix
## and cache it by calling the setinver function defined above.

cacheSolve <- function(x, ...) {
      i <- x$getinver()
      if(!is.null(i)) {
            message("This is the cached data")
            return(i)         ##Return the cached inverse
      }
      matrix <- x$get()
      i <- solve(matrix, ...)
      x$setinver(i)
      i        ## Return a matrix that is the inverse of 'x'
}
