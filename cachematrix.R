## this function will return a list with 4 elements
## each element of the list is a function
## set and get functions will create and retrive the matrix respectively
## setInverse is used to cache the inverse of the matrix
## getInverse is used to retrive the cached data

makeCacheMatrix <- function(mat = matrix()) {
  iMat <- NULL
  
  set <- function(mat_var){
    mat <<- mat_var
    iMat <<- NULL
  }
  get <- function() mat
  
  setInverse <- function(invMatrix) iMat <<- invMatrix
  getInverse <- function() iMat
  
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## this function returns the inverse of the matrix created using makeCacheMatrix() function
## it first checks the cache to see if the inverse matrix is saved or not
## if available it returns the cached data, else it calculates the inverse and saves the data in cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  iMat <- x$getInverse()
  if(!is.null(iMat)){
    message("getting cached data")
    return(iMat)
  }
  
  data <- x$get()
  iMat <- solve(data)
  x$setInverse(iMat)
  iMat
}