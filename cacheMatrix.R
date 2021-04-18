#Pair of functions that cache the inverse of a matrix


###This function creates a special "Matrix" objet that can cache its inverse

makeCacheMatrix <- function(m = matrix()) {
  d <- NULL #Assigs NULL to a variable within the current environment
 
#Set the matrix 
   set <- function(y){
    m <<- matrix
    d <<- NULL
   }

#Get the matrix
  get <- function()m

#Set the inverse of the matrix
  setInverse <- function(inverse) d <<- inverse

#Get the inverse of the matrix
  getInverse <- function() {
    d
  }
  
#List of methods
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

#This function computes the inverse of the special matrix returned by the function above (makeCacheMatrix). 
#Also, if the inverse has already been calculated, then this function below, name cacheSolve retrieves the inverse from the cache.


cacheSolve <- function(m, ...) {
  ## Return a matrix that is the inverse of 'x'
  d <- m$getInverse()
  if(!is.null(d)){
    message("getting cached data")
    return(d)
  }
#Get the matrix from our objet
  datos <- m$get()
  d <- solve(datos) %*% datos
#Set the inverse of the object
  m$setInverse(d)
#Return the matrix
  d
}
