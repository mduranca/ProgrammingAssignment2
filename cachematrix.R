
## The following functions are able to save computational time for the calculation of the inverse of a matrix 
## (assumed it is invertible)

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(A = matrix()) {
    I <- NULL
    set <- function(B) {
        A <<- B
        I <<- NULL
    }
    get <- function() A
    setinv <- function(solve) {
        I <<- solve
    }
    getinv <- function() I
    list(set = set, get = get,
        setinv = setinv,
	getinv = getinv)
}


## This function either calculates the inverse of an invertible matrix or gets it, without need of calculation,
## if it was previously stored.
cachesolve <- function(x, ...) {
    I <- x$getinv() 
    if(!is.null(I)) {                
        message("getting cached data")
	return(I)                
    }
    data <- x$get()                
    I <- solve(data, ...)       
    x$setinv(I) 
    message("inverse has been stored")
    I
}

## returns a matrix that is the inverse of matrix A
