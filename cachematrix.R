## Put comments here that give an overall description of what your
## functions do

# Create a 'special' matrix as a wrapper around the actual matrix x (and
# its inverse x_inv). Keep track of change in value of x (hence x_inv). To
# do this, prohibit direct access to x or x_inv by constructing the 
# special matrix as an encapsulation of accessor functions of x and x_inv,
# only through which one may get/ set them. x and x_inv are referred from
# the closures of these accessor methods.

# When x is initialized or set to a new value, x_inv is set to NULL. 
# Depending on whether x_inv is null or not at the time of inverse 
# computation, the inverse will be newly computed or the precomputed 
# (cached) value of inverse returned. 


## Write a short comment describing this function

# makeCacheMatrix() takes in an actual matrix x and constructs a list of 
# getter and setter methods for x and its inverse x_inv. elements of the 
# list returned are pointers to these methods, thus containing the 
# reference of their respective closures. it is in these closures that x 
# and x_inv are preserved.

makeCacheMatrix <- function(x = matrix()) {
    x_inv <- NULL    
    
    # x and x_inv are set in different environments than the current one,
    # such that their values persist even after exiting set(). the actual
    # objects referred to by x & x_inv are determined by lexical scoping,
    # which finds them in the parent frame of set().
    set <- function(y) {
        x <<- y
        x_inv <<- NULL
    }
    get <- function() {
        x
    }
    
    # like set(), setinverse() also sets x_inv in a different environment
    # so that its value is preserved. 
    setinverse <- function(xi) {
        x_inv <<- xi
    }
    getinverse <- function() {
        x_inv
    }
    
    # return the special matrix as list of functions hence environments, 
    # with its elements named for more intuitive extraction.
    list(get=get, set=set, getinverse=getinverse, setinverse=setinverse)
}


## Write a short comment describing this function

# cacheSolve() computes inverse of special matrix x only if the value of
# x_inv in it is NULL. otherwise, returns the preserved inverse value. the
# value of x_inv is looked up in the closure of methods contained in x. 
# the extra args in the function are passed on to the solve() function.

cacheSolve <- function(x, ...) {
    
    # extract element named 'getinverse' from special matrix x (i.e.list),
    # then evaluate the resulting expression. putting () does so, since
    # the resulting expression is a function. the value evaluated is same
    # as preserved in the environment of getinverse(); assign it to xi.
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
