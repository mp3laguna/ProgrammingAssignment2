
## this function creates an object containing a matrix and four functions to 
## manage the matrix. 
#       set function sets the value matrix and initializes its inverse to null
#       get function returns the matrix
#       setinv function sets the inverse matrix value
#       getinv function returns the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
        
        # initialize the inverse of the matrix as NULL
        inv <- NULL
        
        # function to set the value of the matrix
        set <- function(y){
                #set the value of the matrix in the parent environment
                x <<- y
                #initialize inverse value to NUUL
                inv <<- NULL
        }
        #function that returns the value of the matrix
        get <- function() x
        #function to set the value of the inverted matrix
        setinv <- function(inverse) inv <<- inverse
        #function that returns the stored value of the matrix
        getinv <- function () inv
        #return a list of the four functions
        list(set = set, get = get, setinv = setinv, getinv = getinv)
        
}


#function that returns the inverted value of the special matrix defined above
#if it has already computed the value of the inverse it returns it and sends message 'getting cached data'
#if not computed previously, it calculates the inverse, stores it in the special matricx object
#using 'setinv' function contained in special matrix object and returns it

cacheSolve <- function(x, ...) {
        ## gets the inverse value contained in special matrix object
        inv <- x$getinv()
        #if inverse value is different form NULL it has been computed previously and stored so just returns it
        if(!is.null(inv)){
                message ("getting cached data")
                return(inv)
        }
        #in case not computed prevoiusly, get the value of the matrix
        data <- x$get()
        #compute the inverse value
        inv <- solve(data,...)
        #store result in special matrix object
        x$setinv(inv)
        #return the inverse value
        inv
}