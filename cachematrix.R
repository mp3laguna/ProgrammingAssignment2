## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## this function creates an object containing a matrix and four functions to 
## manage the matrix. 
#       set function sets the value matrix and initializes its inverse to null
#       get function returns the matrix
#       setinv function sets the inverse matrix value
#       getinv function returns the inverted matrix
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function () inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
        
}


## Write a short comment describing this function
## This matrix calculates the inverted matrix by directly calculating it if 
## it is not calculated previously or returning the previous calculated matrix stored in
## makeCacheMatrix object by getinv function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)){
                message ("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinv(inv)
        inv
}