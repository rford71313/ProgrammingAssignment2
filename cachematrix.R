## The following function
## functions do

## Write a short comment describing this function

## makeCacheMatrix takes a matrix as its input. It then creates an object that is a list
## that stores the matrix inputed and what will be the "cached" (stored) inverse of the 
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


## Write a short comment describing this function

## cacheSolve first checks if the inverse has been stored from makeCacheMatrix. If so, it 
## outputs the inverse stored in 'i' as well as a message "getting cached data". If the 
## inverse hasn't been computed, it takes the inverse of the matrix inputed and stores the 
## value of the inverse back in 'i'. Lastly it outputs the inverse 'i'

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