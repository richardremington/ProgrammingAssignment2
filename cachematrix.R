## makeCacheMatrix() and cacheSolve() can be used to together to lower computing
## costs for repeated inversion of square matrices when the the matrix to invert
## is sometimes unchanged from the last matrix to invert.

## makeCacheMatrix() stores a matrix "x", initializes objects "i" and "z" to 
## have NULL values, defines 6 functions and returns them as a list.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  z <- NULL

  getinverse <- function() i
  
  get <- function() x

  setinverse <- function(inverse) i <<- inverse

  setlast <- function(y) z <<- y
  
  getlast <- function() z
  
  set <- function(x) x <<- x
    
  list( getinverse = getinverse,
        get = get, 
        setinverse = setinverse,
        setlast =  setlast, 
        getlast = getlast, 
        set = set)
}


## cacheSolve() gets an inverse from a makeCacheMatrix object.  

## (1)  If the inverse is NULL, then the function gets a matrix from the 
## makeCacheMatrix object, inverts the matrix, caches the inverse and the matrix 
## in the environment of the makeCacheMatrix object, and returns the inverse.  

## (2)  If the inverse is not NULL, then the function gets
## the cached matrix and the matrix to invert.  If the matrices are the same, 
## the function returns the cached inverse.  If the matrices are different, 
## then function inverts the new matrix, caches the inverse and the new matrix 
## in the environment of the makeCacheMatrix object, and returns the inverse.  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()  
  
  # 1st call to cacheSolve()
  if(is.null(i))
  {
    data <- x$get()
    
    i <- solve(data, ...)
    
    x$setinverse(i)
    
    x$setlast(data)
    
    return(i)
  }
  
  # subseqent calls to cacheSolve()
  z <- x$getlast()
  data <- x$get()
  if( identical( z, data, ignore.environment = TRUE))
  {  
    message("getting cached data")
    
    return(i)
  }
  else
  {
    i <- solve(data, ...)
    
    x$setinverse(i)
    
    x$setlast(data)
    
    return(i)
  }    
}
