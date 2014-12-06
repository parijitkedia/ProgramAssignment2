## Programming Assignment 2
## R Programming
## Coursera Specialization 2nd Course

## This function creates an enhanced matrix that contains
## functions for storing and retrieving the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        ## setmatrix will set the matrix of type makeCacheMatrix and initialize its inverse
        setmatrix <- function(y) {
        x <<- y
        inv <<- NULL
        }
        ## getmatrix will get the matrix of type makeCacheMatrix
        
        
        getmatrix <- function() x
        
        ## getinverse will get the inverse of the matrix
        ## setinverse will set the inverse of the matrix
        setinverse <- function(inverse) inv <<- inverse
        
        # Gets the inverse of the matrix, inv_x
        getinverse <- function() inv
        list(setmatrix = setmatrix, getmatrix = getmatrix,
        setinverse = setinverse, getinverse = getinverse)

}

## This function uses the enhanced cache matrix to return the inverse

## If the inverse has already been calculated then it simply looks up and returns

##Otherwise, the inverse is calculated and stored in the cacheMatrix and then returned.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        
        # Check to see if the inverse of matrix is NOT NUL0L
        # If it's NOT NULL, return inv_x as retrieved above and ends cacheSolve function
        
        if(!is.null(inv)) {
        message("getting cached inverse of matrix")
        return(inv)
        }
         # Inverse of matrix is NULL so calculate and set it
        inv <- solve(x$getmatrix(), ...)
        x$setinverse(inv)
        inv
}
