##The following functions, makeCacheMatrix and cacheSolve help
##in caching and solving a inverse of a matrix.
## Usage of this functions is shown below
## m1 = matrix ( c(1,2,3,4) , nrow = 2 , ncol =2 )
## m2 = matrix ( c(10,20,30,40), nrow =2, ncol =2 )
## l <- list ( makeCacheMatrix(m1) , makeCacheMatrix(m2))
## inverses <- lapply( l, cacheSolve )
## inverses 
## inverses will return a list of inverse matrices

## makeCacheMatrix can be thought about as a wrapper around 
## the actual matrox we need to operate upon. 
## Think of it a object that has can return a set of functions 
##  get() -> Returns the matrix itself
##  set() -> Caches the matrix in the 'separate environment' 
##  setInverse() -> does not calculate the inverse itself, but stores it
##  getInverse() -> returns the cached inverse of the matrix if present, else NULL
##
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y ## Matrix which is the key to lookup and stored in a different environment.
    m <<- NULL 
  }
  get <- function() x ## getter used by list. 
  
  ## the setter which can be invoked as a element of a vector.
  setInverse <- function(inverseMatrix) m <<- inverseMatrix 
  
  ## getter to the inverse of the cache matrix.
  getInverse <- function() m 
  
  ## this is a set of vectors that are nothing but functions. 
  ## R is close to functional programming, Think of this is a higher order
  ## function i.e. vector of elements that return a function.
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse) 
}


## cacheSolve is a function that takes in a 'special matix' as a input.
## It looks up the matrix in the cache - i.e. it executes a function to see
## if the matrix is in the cache, if found it returns the inverse from the cache.
## Else, it computes the inverse of the matrix using `solve`, calls the 
## setter function on the special matrix object that is passed in 

cacheSolve <- function( x = makeCacheMatrix()) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse() ## Do we have the inverse cache ?
  if(!is.null(m)) { ## oh yes ,Return it
    message("getting cached matrix")
    return(m)
  }
  data <- x$get() ## get the Matix 
  m <- solve(data)
  message("caching computed inverse")
  x$setInverse(m)
  m  
}

