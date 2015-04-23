## The following functions get and set the inverse of a matrix. If the inverse doesn't exist
## it is created and cached. If it does exist, the cached value is retrieved and returned
## To test:
## > mc <- makeCacheMatrix(matrix(c(2, 3, 2, 1, 2, 1, 1, 1, 2), 3))
## > cacheSolve(mc)


## makeCacheMatrix creates a special matrix which is a list containing the following getter and setter functions:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix 
## get the value of the inverse of the matrix 



makeCacheMatrix <- function(x = matrix()) {
        i <- NULL ## i is the inverse of the matrix. Initialize it to null
        set <- function(y) { ## this function stores the passed in matrix  
                x <<- y  
                i <<- NULL
        }
        get <- function() x ## this function returns the stored matrix
        setinverse <- function(solve) i <<- solve ## this function stores the passed in value of the inverse of the matrix into the variable "i" 
        getinverse <- function() i ## this function returns the value stored in "i"
        list(set = set, get = get, ## this function stores the above defined functions in a list 
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if the
## inverse has already been calculated. If so, it gets the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of
## the input matrix and sets the value of the inverse in the cache via the setinverse function

cacheSolve <- function(x, ...) {
        i <- x$getinverse() ## invoke the getinverse function to get the inverse of the matrix
        if(!is.null(i)) { ## return the cached value if it exists
                message("getting cached data")
                return(i)
        }
        data <- x$get() ## if no cached value exists, calculate it 
        i <- solve(data, ...)
        x$setinverse(i) ## cache calculated value
        i
}
