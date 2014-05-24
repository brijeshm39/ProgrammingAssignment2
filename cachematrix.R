

## Creates a special 'matrix' object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  k  <- NULL
  set  <- function(y){
          x <<- y
          k <<- NULL 
  }
  get  <- function() x
  setinverse  <- function(inverse) k  <<- inverse
  getinverse  <- function() k
  list(set= set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)

}


#If inverse has already calculated then retrieves the inverse from the cache
#else It computes the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         k  <- x$getinverse()
        if (!is.null(k)){
                message("getting cached data")
                return(k)
        }
        data  <- x$get()
        k  <- solve(data, ...)
        x$setinverse(k)
        k
}
