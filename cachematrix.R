## Put comments here that give an overall description of what your
## functions do
##Cousera R Programming, Jun 2015, @ytang

## Write a short comment describing this function
makeCacheMatrix <- function(m = matrix()) {
	##take in a matrix (set), get its inverse (get_inv) if available
	#input: a matrix m
	#output: a list containing 4 elements: set, get, setinv, getinv 
	#m: matrix; inv:inverse of the matrix m 

	inv <- NULL #set the initial value of the inv is NULL
	set <- function(x) {  
		m <<- x #assign the matrix to variable m
		inv <<- NULL  #initially, the inverse of the matrix is NULL
	}
	get <- function() m  #get the matrix m
	setinv <- function(inv_m) inv <<- inv_m  #set the inverse value of m
	getinv <- function() inv  #cache the inverse value of m
	
	#output of the function: a list of 4 elements
	list(set = set, get = get,
		setinv = setinv,
		getinv = getinv)		
}

## Write a short comment describing this function
cacheSolve <- function(x, ...) {
        ## This functions returns an inverse of a matrix
		## input: the output of the function "makeCacheMatrix"
		## output: matrix inversion, either cache if was previously calculated
		
		inv <- x$getinv()
		#return inversion from cache if available already
		if(!is.null(inv)){
			message("gettting cached data")
			return(inv)
		}
		#compute and return the inversion if not available 
		data <- x$get()
		inv <- solve(data, ...) 
		x$setinv(inv)
		inv
}

