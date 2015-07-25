
## this function returns a List which can hold a matrix and its calculated inverse in cache

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
set <- function(y){
  x <<- y
  m <<- NULL
}
get <- function() x
setinverse <- function(solve) m <<-solve
getinverse <- function() m
list(set = set, get = get,
     setinverse = setinverse,
     getinverse = getinverse)
}


## This function returns a matrix that is the inverse of 'x'
## It checks that there is an inverse for the matrix 'x' in cache
## If not, calculate inverse of matrix in the List x

cacheSolve <- function(x, ...) {
       
  m <- x$getinverse()
  data <- x$get()
  if(!is.null(m)) {
    ## check inverse matrix in cache is an inverse of the matrix in 'x'
    if ((data%*%m)%*%data == data) {
      message("getting cached data")
      return(m)
      }
    } 
  
  m <- solve(data)
  x$setinverse(m)
  m
}
