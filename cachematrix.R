## Put comments here that give an overall description of what your
## functions do
## These functions were designed to cache the inverse of a matrix to recall
## it in the future quickly.

## Write a short comment describing this function
## This function is designed to set and get both the value of a matrix
## and it's inverse (that is, the matrix that when multiplied with the
## original matrix, will present the identity matrix)

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Write a short comment describing this function
## This function is designed to call the inverse resource from the cache.
## If the inverse resource does NOT exist, it will be calculated, and then cached

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached inverse")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
	  }
