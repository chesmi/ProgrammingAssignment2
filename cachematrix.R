## Below are two functions that are used to create a special object
## that stores a matrix (square) and cache's its inverse
###################################################################

## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to
########## 1. set the value of the matrix
########## 2. get the value of the matrix
########## 3. set the value of the inverse
########## 4. get the value of the inverse

## The second function, cacheSolve computes the inverse of the special "matrix" 
## returned by makeCacheMatrix function.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {

 i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)


}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
