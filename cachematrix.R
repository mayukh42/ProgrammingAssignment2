## Put comments here that give an overall description of what your
## functions do

# Create a 'special' matrix as a wrapper around the actual matrix x (and
# its inverse x_inv). When x is initialized or set to a new value, x_inv
# is set to NULL. Depending on whether x_inv is null or not at the time 
# of inverse computation, the inverse will be newly computed or the 
# precomputed (cached) value of inverse returned. 

# To keep track of the change in value of actual matrix x (hence x_inv),
# direct access to them is prohibited. Instead, the special matrix is 
# designed as an encapsulation of accessor methods, only through which 
# one may access the actual matrix or its inverse.

## Write a short comment describing this function

# makeCacheMatrix() takes in an actual matrix x and constructs a list of 
# getter and setter methods for x and its inverse x_inv. x and x_inv can
# only be accessed through these methods.

makeCacheMatrix <- function(x = matrix()) {
    x_inv <- NULL    
    
    # x and x_inv are always set in different environments than the 
    # current one, such that their values persist even after exiting 
    # either set() or makeCacheMatrix() environments.
    set <- function(y) {
        x <<- y
        x_inv <<- NULL
    }
    get <- function() {
        x
    }
    
    # like set(), setinverse() also sets x_inv in a different 
    # environment so that its value is preserved.
    setinverse <- function(xi) {
        x_inv <<- xi
    }
    getinverse <- function() {
        x_inv
    }
    
    # return the special matrix as a list, with its elements named for 
    # more intuitive extraction.
    list(get=get, set=set, getinverse=getinverse, setinverse=setinverse)
}


## Write a short comment describing this function

# cacheSolve() computes inverse of special matrix x only if the value of
# x_inv in it is NULL. otherwise, returns the existing inverse value. the
# extra args in the function are passed on to the solve() function.

cacheSolve <- function(x, ...) {
    
    # extract element named 'getinverse' from special matrix x (i.e.list),
    # then, evaluate the resulting expression. putting () does so, since
    # the resulting expression is a function. assign that value to xi.
    xi <- x$getinverse()
    if (!is.null(xi)) {
        message("getting cached inverse")
        return(xi)
    }
    
    data <- x$get()
    message("calculating new inverse")
    xi <- solve(data, ...)
    x$setinverse(xi)
    
    ## Return a matrix that is the inverse of 'x'
    xi
}
