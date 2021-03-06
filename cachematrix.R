## The following functions compute and cache (store) the inverse of a matrix inputed. The first 
## function makeCacheMatrix creates a special matrix object that can cache its inverse. The 
## second function cacheSolve computes the inverse of the special matrix returned by makeCacheMatrix.
## If the inverse has already been calculated, then the cachesolve should retrieve the inverse 
## from the cache.


## Function 'makeCacheMatrix' takes a matrix as its input. It then creates an object that is a 
## list that stores the matrix inputed and what will be the cached (stored) inverse of the 
## matrix. When called by cacheSolve, the function will either get the value of the 
## original matrix inputed to compute the inverse or get the inverse computed that is stored 
## in 'i'. It then stores the inverse as the cached value into 'i'.

makeCacheMatrix <- function(x = matrix()) {
 
	i <- NULL
  
  	set <- function(y) {    
    		x <<- y        	  
    		i <<- NULL    	  
	 }
   
  	get <- function() { x }   
  
  	setsolve <- function(solve)  { i <<- solve }				 
  
  	getsolve <- function() { i } 
  
  list(get = get,           
       setsolve = setsolve,
       getsolve = getsolve)
}



## Function cacheSolve checks if the inverse has been stored from makeCacheMatrix. If it was 
## previously computed and stored, it outputs the inverse stored in 'i' as well as a message 
## "getting cached data". If the inverse hasn't been computed, it takes the inverse of the matrix 
## inputed and stores the value of the inverse back in 'i'. Lastly it outputs the inverse 'i'

cacheSolve <- function(x, ...) {
  
	i <- x$getsolve()
  
	if(!is.null(i)) {
    
    		message("getting cached data")
    		return(i)
	}
	
	data <- x$get()
	i <- solve(data, ...)
 	x$setsolve(i)
  i
}