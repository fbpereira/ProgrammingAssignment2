## Matrix Inversion
## The following functions are able to give you the inverse of a square matrix.

## The makeCacheMatrix is a function that will create a special "matrix" object that can cache its inverse.
## Attention: create an object to attribute the special "matrix" object. It will be used in the next function.

makeCacheMatrix <- function(x = matrix()) {
  m.inverse <- NULL
        set <- function(y){ 
                x <<-y
                m.inverse <<- NULL}
        get <- function() x
        set_inverse <- function(inverse) m.inverse <<- inverse
        get_inverse <- function() m.inverse
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)

}


##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
## Use the special "matrix" object to obtain the inverse of the original matrix or you can nest the functions (cacheSolve(makeCacheMatrix(x)).

cacheSolve <- function(x, ...) {
  m.inverse<-x$get_inverse()
  
  if(!is.null(m.inverse)){
    message("Getting cached data")
    return(m.inverse)}
  
  matrix <- x$get()
  m.inverse <- solve(matrix,...)
  x$set_inverse(m.inverse)
  m.inverse
}

## I believe that is easiear create another function that automatically nest the above functions, so you can use the following:

Inverse_matrix <- function(x,...){
  cacheSolve(makeCacheMatrix(x))
}
