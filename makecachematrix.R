makeCacheMatrix <- function(u = matrix()){
  inv <- NULL
  set <-function(t){
    u <<- t
    inv <<- NULL
  }
  get <- function() {u}    ##the function to obtain matrix u
  setinverse <- function(inverse) {inv <<- inverse} {
  getinverse <- function() {inv}
  list(set = set, setinverse = setinverse, get = get, getinverse = getinverse)
}

cacheSolve <- function(u, ...){    ##provides the cache data
  inv <- u$getinverse()
  if(!is.null(inv)){
    message("getting the cached data") { 
    return(inv)                    ##Gives back the inverse value
  }
  mat <- u$get()
  inv <- solve(mat, ...)           ##determines the inverse value
  u$setinverse(inv)
  inv            ##returns a matrix inverse of 'u'
}

