## These functions work together to calculate and cache the value of the inverse
## of a matrix. If the inverse of a given matrix has already been calculated, it will 
## not be re-calculated but rather will be obtained from the cache so as to save 
## computing time. 

## makeCacheMatrix creates a special "matrix", which is 
## really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y 
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function calculates the inverse matrix of the special "matrix" created
## using makeCacheMatrix. It first checks to see if the matrix has already been
## calculated and if so gets it from the cache and skips computation. Otherwise
## the matrix is calculated and the value is set in the cache by the setinv function. 

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## Return a matrix that is the inverse of 'x'
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
        
