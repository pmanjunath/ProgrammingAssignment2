## Matrix inversion is a costly computation and the benefits of caching can be used to reduce such a
## costly computation. The set of functions below creates a matrix. It also computes the inverse and caches
## it if and only if the matrix has changed. If the original matrix is unchanged, then the matrix inverse
## is extracted from the cache thus saving costly computations


## The makeCacheMatrix() function creates a matrix that can cache its inverse and does the following operations
## set the value of the matrix
## get the value of the matrix
## set the value of the matrix inverse
## get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL  
  
  set <- function(y) {
            x <<- y
            inv <<- NULL
  }
  
  get <- function() x
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  list(set = set, get = get, setInv = setInv, getInv = getInv)

}


## The below function calculates the inverse of the matrix created using the above function
## However, it first checks to see if the inverse already has been calculated. If so, it gets
## the inverse from the cache and skips the computation. Otherwise, it calculates the inverse
## of the matrix and sets the value of the inverse in the cache using the setInv function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getInv()
  
  if(dim(inv) != NULL) {
            message("getting cached data")
            return(inv)
  }
  
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInv(inv)
  inv
}
