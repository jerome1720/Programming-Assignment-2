makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <-function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}    ##the function to obtain matrix x
  setinverse <- function(inverse) {inv <<- inverse}
  getinverse <- function() {inv}
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

cacheSolve <- function(x, ...){    ##provides the cache data
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)                    ##Gives back the inverse value
  }
  mat <- x$get()
  inv <- solve(mat, ...)           ##determines the inverse value
  x$setinverse(inv)
  inv            ##returns a matrix inverse of 'x'
}

