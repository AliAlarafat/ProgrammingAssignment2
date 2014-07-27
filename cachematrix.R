 ## This function computes the inverse of the special
 ## "matrix" returned by `makeCacheMatrix`as variable x. If the inverse has
 ##  already been calculated (and the matrix has not changed), then the
 ## `cachesolve` should retrieve the inverse from the cache.
 ##  After retrieving the inverse it will be assigned to variable m
 ##  Variable m will not be returned but rather set to x then printed


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
  x <<- y
  m <<- NULL
}

get <- function() x
setmatrix <- function(solve) m<<- solve
getmatrix <- function() m
list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}



cacheSolve <- function(x = matrix(), ...) {
    m<-x$getmatrix()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    matrix <- x$get()
    m <- solve(matrix, ...)
    x$setmatrix(m)
    m
}

 ## I struggled a lot with this and used as much help I can from the instructor and fellow students
 ## This is my best attempt at solving the assignment. Thank you.