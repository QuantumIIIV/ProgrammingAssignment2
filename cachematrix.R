## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function
## This is a function identical to one poseted on the Programming Assignment 2: Lexical Scoping
## I changed a few item and rplaced with Inv
makeCacheMatrix <- function(x = matrix()) {
        Inv <- NULL                     #Reset the content
        set <- function (y){            #Define the function "set"
                x <<- y
                Inv <<- NULL
        }
                
        get <- function () x            # Define function "get"
        setinv <- function (Inverse1) Inv <<- Inverse1
        getinv <- function() Inv
        
        list(set=set, get=get, 
             setinv=setinv, 
             getinv=getinv)
}

## Write a short comment describing this function
##return Cached Inv if exist, if not caluculate.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        Inv <- x$getinv()
        if (!is.null(Inv)) {
                message ("Getting Cached data")
                return (Inv)
        }
        data <- x$get()
        Inv <- solve(data, ...)                 #Calcualte the inverse of a diagonal Matrix
        x$setinv(Inv)    
        Inv
}