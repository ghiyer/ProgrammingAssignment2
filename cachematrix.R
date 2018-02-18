install.packages("matrixcalc")
library("matrixcalc")
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  dim<-length(data)
  dim<-sqrt(dim)
  if (ceiling(dim)!=dim) {
    print("The matrix is not a square matrix - No Inverse")
    # break
  }
  else {
   myMatrix = matrix(data, dim, dim, byrow=TRUE )
   if (is.singular.matrix(myMatrix)) {
     print("The matrix is a singular matrix - No Inverse")
   }
   else {
     inv <- solve(myMatrix, ...)
     x$setinverse(inv)
     print("Here is the inverse of the matrix")
     inv
   }
  }
}

