makeCacheMatrix <- function(x = matrix()) {
  ## @x: a square invertible matrix
  ## return: a list containing functions to
  ##              1. set the matrix
  ##              2. get the matrix
  ##              3. set the inverse
  ##              4. get the inverse
  
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse 
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
  ## return: inverse of the original matrix input to makeCacheMatrix()
  
  inv <- x$getinv()
  
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  # otherwise, calculates the inverse 
  mat.data = x$get()
  inv <- solve(mat.data, ...)

  x$setinv(inv)
  
  return(inv)
}

test = function(mat){
  ## @mat: an invertible matrix
  
  temp = makeCacheMatrix(mat)
  
  start.time = Sys.time()
  cacheSolve(temp)
  dur = Sys.time() - start.time
  print(dur)
  
  start.time = Sys.time()
  cacheSolve(temp)
  dur = Sys.time() - start.time
  print(dur)
}
