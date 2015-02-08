## makeCacheMatrix() creates a special "matrix" object and caches its inverse
## cacheSolve() computes the inverse of the special "matrix" and retrieves the inverse
## from the cache if the inverse has been calculated and the matrix has not changed 

## makeCacheMatrix() creates a special matrix object and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
    
     # 1. Set the value of the "matrix"

     set <- function(y) {
          x <<- y
 	  m <<- NULL
     }

     ## 2. Get the value of the "matrix"
     get <- function() x

     ## 3. set the inverse the "matrix"
     setinv <- function(solve) m <- solve

     ## 4. get the inverse of the "matrix"
     getinv < function() m
     list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## cacheSolve calculates the inverse of the special "matrix" returned by makeCaheMatrix.
# if the inverse has been already cached and the matrix didn't change, cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
          m <- x$getinv  ## Return a matrix that is the inverse of 'x' 
         
         ## Checks if the inverse is already in the cache then return the inverse
         if(!is.null(m)) {
               
           message("getting inverse from the cache")
      
           return(m)

          }     

	## Otherwise, computes the inverse of the matrix
	data <- x$get()

        m<- solve(data, ...)
   
    
        ## And sets the inverse in the cache 
        x$setinv(m)
    
        m
}
