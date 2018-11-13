## these functions combined allow you to input a matrix, compute/return its 
## inverse, and cache that inverse for repeated use

## this function sets up a list of functions to be used on a matrix (x)
## these functions can be called on later using x$ operator, particularly
## within later cacheSolve function. x is empty matrix, s NULL to start

makeCacheMatrix <- function(x = matrix(), ...) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setinv <- function(solve) s <<- solve
        getinv <- function() s
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## this takes a makeCacheMatrix type matrix and either a) if s is not null,
## returns the cached inverse matrix previously computed, or b) computes
## the inverted matrix on its own. you can tell which it was doing by whether
## or not it returns to you the note that it was using cached data

cacheSolve <- function(x, ...) {
        s <- x$getinv()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinv(s)
        s
}
