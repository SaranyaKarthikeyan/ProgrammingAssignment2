# The function "makeCacheMatrix" creates a special matrix 
# which can cache the inverse of a matrix
# The function first get and set the value of the matrix
# followed by getting and setting the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  Inv <- NULL
  set <- function(y) {
    x <<- y
    Inv <<- NULL
  }
  get <- function()x
  setInverse <- function(Inverse) Inv <<- Inverse
  getInverse <- function() Inv
  list (get = get, set = set, getInverse = getInverse, setInverse = setInverse)
  
}



# The following "cacheSolve" function computes the inverse of the matrix 
# if it is not already computed and Cache the value of Inverse of that matrix
# In case, inverse is already computed, it returns the computed value

cacheSolve <- function(x, ...) {
  Inv <- x$getInverse()
  if(!is.null(Inv)) {
    message("getting cached data..")
	return(Inv)
  }
  data <- x$get()
  Inv <- solve(data, ...)
  x$setInverse(Inv)
  Inv
}

## Sample Output

> x = matrix(1:4, 2, 2)
> m = makeCacheMatrix(x)
> m$get()
     [,1] [,2]
[1,]    1    3
[2,]    2    4
> 
> # first run
> cacheSolve(m)
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> 
> # second run
> cacheSolve(m)
getting cached data..
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> 
