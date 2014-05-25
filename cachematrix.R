## Functions to cache the inverse of a matrix.
## The computation of the inverse of a matrix
## can be timeconsuming with these functions
## it will only be necessary to do i once.

## makeCacheMatrix creates a special matrix,
## which really is a list containing funtions
## to set the value of the matrix, get the value
## of the matrix set the value of the inverse
## and get the value of the inverse. 

makeCacheMatrix <- function(x = matrix()) {
    
    #initialises the matrix
    m <- NULL
    
    # defining the 4 functions
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    
    # the return value
    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse) 
  }
  
  


## cacheSolve returns the inverse of a cached matrix.
## The function checks if the inverse allready has been
## computed, if this is the case the cached inverse is
## returned, otherwise the inverse is returned.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    
    # check if inverse is cumputed
    if(!is.null(m)) {
        message("getting cached data")
        
        # return value (cached version)
        return(m)  
    }
    
    # computation of inverse if no cache inverse exist
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    
    # the return value (computed first time)
    m 
  }
