## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(m=matrix()) {
  i <- NULL #sets inverse to null
  set <- function(matrix) {
    m <<- matrix
    i <<- NULL
  } #set the matrix
  get <- function() {m} #get the matrix
  setinverse <- function(inverse) {i <<- inverse} #set inverse of matrix
  getinverse <- function() {i} #get inverse of matrix
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) #returns list of the above 4 functions
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinverse() #return a matrix that is the inverse of 'x'
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  } #return the inverse if inverse is already calculated
  data <- x$get() #get the matrix
  i <- solve(data, ...) #calculate the inverse of the matrix
  x$setinverse(i) #set inverse
  i #return inverse matrix
}