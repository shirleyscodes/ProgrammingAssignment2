## Caching the Mean of a Vector [Shirley Asangi]


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<-y
    inv <<-NULL
  }
  get <- function()x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get = get,
       setinverse = setinverse,
       getinverse=getinverse
       )

}


## Write a short comment describing this function

cacheinverse <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix_to_inverse <- x$get()
  inv <- solve(matrix_to_inverse, ...)
  x$setinverse(inv)
  inv
      ## Return a matrix that is the inverse of 'x'
}
