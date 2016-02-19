## The following is a pair of functions that cache the inverse of a matrix.

## The function 'makeCacheMatrix' creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
  x<<-y
  m<<-NULL
}
get<-function() x
set_mat<-function(solve) m<<- solve
get_mat<-function() m
list(set=set, get=get,
   set_mat=set_mat,
   get_mat=get_mat)
}

## The function 'cacheSolve' computes the inverse of the special "matrix" returned by 'makeCacheMatrix' function above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$get_mat()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    mat<-x$get()
    m<-solve(mat)
    x$set_mat(m)
    m
}

