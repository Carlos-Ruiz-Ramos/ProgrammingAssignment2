## Put comments here that give an overall description of what your
## functions do

# Example 
# Matriz <- matrix(c(0,-1,1,0), nrow=2, ncol=2, byrow =  TRUE)
# Construccion = makeCacheMatrix(Matriz)
# cacheSolve(Construccion)
#     [,1] [,2]
#[1,]    0    1
#[2,]   -1    0
# cacheSolve(Construccion)
#Inverso
#     [,1] [,2]
#[1,]    0    1
#[2,]   -1    0

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  get <- function() x
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # Get and Set of Inverso
  getInverso <- function() m
  setInverso <- function(inverse) m <<- inverse
  
  list(set = set,
       get = get,
       setInverso = setInverso,
       getInverso = getInverso
       )
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {

  m <- x$getInverso()
  
  #Show message if cached
  if(!is.null(m)) {
    message("Inverso")
    return(m)
  }
  
  # Inverse of matrix
  data <- x$get()
  m <- solve(data, ...)
  
  x$setInverso(m)
  # Return a matrix that is the inverse of 'x'
  m
}
