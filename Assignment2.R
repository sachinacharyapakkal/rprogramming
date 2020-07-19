##R Programming
##Week3 Assignment

##makeCacheMatrix
makeCacheMatrix<-function(x=matrix()) {
  m<-NULL
  set<-function(y) {
    x <<- y
    m <<- NULL
  }
  get<-function() x
  setinverse<-function(inverse) m <<-inverse
  getinverse<-function() m
  list(set=set,
       get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}

##CacheSolve
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

rko<-makeCacheMatrix(matrix(c(2,4,6,8),2,2))
print(rko$get())
cacheSolve(rko)

