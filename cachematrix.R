## This is a brief description for the Week 3 Peer Review Assignment
## R Programming Course

## makeCacheMatrix is a function that returns a list of functions
## It serves to store a martix and cached value of its inverse 
## The content of the function is as follows:
## > setMatrix      sets the value of a matrix
## > getMatrix      gets the value of a matrix
## > cacheInverse   gets the cahced value (inverse of the matrix)
## > getInverse     gets the cahced value (inverse of the matrix)


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() {x}
        setInverse <- function(inverse) {inv <<- inverse}
        getInverse <- function() {inv}
        list(set - set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## The function below computes the inverse of a matrix returned by 
## the function above 

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat,...)
        x$setInverse(inv)
        inv
}

## Given the assumptions of the assignment, 
## the matrix supplied is always invertible 