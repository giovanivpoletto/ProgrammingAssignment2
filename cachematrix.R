## Here they are: two functions who calculates an inverse matrix and cache the result for a second computation

## This code is based on makeVector and cachemean codes presented as an example for the Programming Assignment 2

## This function below caches a matrix and the inversed os this matrix using other functions to seting only a matrix,
## get this matrix, set the inversed matrix, get this solved matrix and cache as a list.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                if(is.matrix(y)){ ## control if it is a matrix passed as argument
                        x <<- y
                        m <<- NULL
                }else{                  ## if it's not returns a message
                        print("You don't pass a matrix, please try it again!")
                }
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function calculates the inverse matrix, present the results and call the function makeCacheMatrix above to cache it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting data from cache")
                return(m)
        }
        z <- x$get()
        m <- solve(z, ...)
        x$setsolve(m)
        m
}