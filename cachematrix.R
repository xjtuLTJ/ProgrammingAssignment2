## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse;
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above, 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
## 

## The first function, makeCacheMatrix, creates a special "matrix", which is really a list containing a function to

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## The following function calculates inverse of the special "matrix" created with the above function. 
## However, it first checks to see if inverse of the matrix has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. Otherwise, 
## it calculates the inverse in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
