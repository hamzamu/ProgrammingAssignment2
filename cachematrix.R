
#Matrix inversion is usually a costly computation , here we will be caching the inverse of a matrix 
#rather than compute it repeatedly .


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


##TESTING .

#------ Setting Matrix .
#> x <- matrix(1:4 , 2 , 2)
#> x
#[,1] [,2]
#[1,]    1    3
#[2,]    2    4


# m <- makeCacheMatrix(x)
# cacheSolve(m)
#----
#getting cached data
#[,1] [,2]
#[1,]   -2  1.5
#[2,] 

#---Solving the matrix---

#> solve(x)
#----
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5