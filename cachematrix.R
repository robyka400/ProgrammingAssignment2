makeCacheMatrix <- function (x = matrix()) # default constructor is 
  #a 1:1 matrix containing an NA
{
  inverse <- NULL     #inverse is by default NULL, 
  #because we don't know yet the inverse of the matrix
  
  set <- function(y)
  {
    x <<- y           #changing the value of the matrix
    inverse <<- NULL   #we don't know yet the inverse of the matrix
  }
  get <- function() x     #returns the current matrix
  setInverse <- function( inv ) inverse <<- inv  #changes the inverse of the matrix
  getInverse <- function() inverse   #returns the inverse of the matrix
  
  list( set = set, get = get, 
        setInverse = setInverse, #returns the list with the functions
        getInverse = getInverse)
  
}

cacheSolve <- function (x)
{
  inverse <- x$getInverse()   
  if(!is.null(inverse))
  { 
    message("getting cached data")  #checks if the inverse matrix 
    return(inverse)                 #has already been calculated
  }
  
  m <- x$get
  inverse <- solve(m)     #calculates the inverse of the matrix
  x$setInverse(inverse)   #saves in the cache 
  inverse                 #and returns the invers
}