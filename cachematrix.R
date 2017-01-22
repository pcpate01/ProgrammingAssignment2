# makeCacheMatrix has the following steps:
# 1. setting the value of the matrix
# 2. getting (requesting) the value of the matrix
# 3. setting the value of inverse of the matrix
# 4. getting (requesting) the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# In the next function the inverse of the matrix it returned. After the initial check on whether 
# the inverse the inverse if already claculated the function gets the result and
# the calculaiton is skiped else the inverse is calcualted and the result is cached in 
# setinverse function.

# Starting with the assumption that the matrix is always invertible

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("obtaining data in cache")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}


## Trial run:
#> x = rbind(c(4, 1/16), c(-4/16, 4))
#> x
#[,1]   [,2]
#[1,]  4.00 0.0625
#[2,] -0.25 4.0000
#> m = makeCacheMatrix(x)
#> m$get()
#[,1]   [,2]
#[1,]  4.00 0.0625
#[2,] -0.25 4.0000
#> cacheSolve(m)
#[,1]         [,2]
#[1,] 0.24975610 -0.003902439
#[2,] 0.01560976  0.249756098
#> cacheSolve(m)
#obtaining data in cache
#[,1]         [,2]
#[1,] 0.24975610 -0.003902439
#[2,] 0.01560976  0.249756098

