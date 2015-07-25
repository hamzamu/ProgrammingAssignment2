

## makeCacheMatrix

### Creating list of functions for setting and getting the matrix and it's inverse .
# set : Setting the matrix .
# get : Getting the matrix .
# setInverse : Setting the inverse of the matrix .
# getInverse : Getting the inverse of the matrix .



makeCacheMatrix <- function(x = matrix()) {
  
  
  inv <- NULL 
  
  set <- function(m){
      x <<- m
      inv <- NULL
  }
  
  get <- function () x 
  
  setInverse <- function(inverse)  inv <<- inverse
  
  getInverse <- function() inv
  
  list(
    set = set ,
    get = get ,
    setInverse = setInverse  ,
    getInverse = getInverse 
  )
  
}


## cacheSolve

## cacheSolve is a function to return the inverse of the matrix .. first it's getting the inverse value ,
## then  validate the if the inverse value , if it's not null it return the inverse value . 
## if inverse is NULL , it skip to getting the matrix , computing it then setting the inverse and return it .

cacheSolve <- function(x, ...) {
  
  inverse <- x$getinverse()
  
  if(!is.null(inverse)) {
    
    message("getting cached data.")
    
    return(inverse)
  }
  
  data <- x$get()
  
  inverse <- solve(data)
  
  x$setinverse(inverse)
  
  inverse
}
