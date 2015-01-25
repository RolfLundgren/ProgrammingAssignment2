## These funcitons work together to cache the inverse
## of a square invertible matrix.


## makeCacheMatrix() receives a (square invertible) matrix as an argument and
## returns a list of four function closures. This list of closures may then be
## passed to cacheSolve().

makeCacheMatrix <- function(x = matrix()) {     # Sets the default argument as
                                                # a matrix.
        
        m <- NULL       # Initializes the 'm' variable as NULL, which
                        # is important in the 'if' statement of cacheSolve().
                        # The "NULL" status indicates the inverse matrix value
                        # has not been cached yet.
        
        # The set() function sets the "x" variable as the matrix we wish to use
        # as an argument in the *parent environment*. This allows us to call the
        # ~$set() function to identify or change the matrix we wish to cache in 
        # the cacheSolve() function without running the makeCacheMatrix()
        # function again. In such an instance, it also resets the "m" variable
        # to NULL in the *parent environment*.
        set <- function(y) {    
                                
                x <<- y
                m <<- NULL
        }
        get <- function() x # Returns the value of the matrix in question.
        
        setinverse <- function(solve) m <<- solve # Sets the value of "m" in
        # the *parent environment* to the inverse of the matrix in question
        # [which is calculated in cacheSolve()].
        
        getinverse <- function() m # Returns the inverse matrix value.
        
        # Returns a list of functions with defined environments (closures).
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## cacheSolve() takes the list created by makeCacheMatrix as an argument and
## returns the cached value of the inverse matrix (if present). If there is no
## cached value, it solves the matrix and caches the value.

cacheSolve <- function(x, ...) {
        m <- x$getinverse() # Assigns the cached value to "m" if it exists. If
        # it has not been set, it will be NULL.
        
        # If the cached value is not NULL (i.e. the matrix has been solved and
        # cached already), this returns that cached value.
        if(!is.null(m)) {
                message("retrieving cached data...")
                return(m)
                
        } else { # If the "m" value is NULL, indicating that the matrix has not
                 # yet been solved or cached, the following steps occur...
                
                data <- x$get() # Assigns the matrix in question to "data".
                
                m <- solve(data, ...) # Solves matrix and assigns it to "m".
                
                x$setinverse(m) # Caches solved matrix value by calling
                                # ~$setinverse() function [see line 30].
        
                m # Returns inverted matrix.
        }
}