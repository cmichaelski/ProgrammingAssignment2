## makeCacheMatrix creates matrix object that can cache its inverse
## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix
## If the inverse has already been calculated, then cacheSolve retrieves the 
## inverse from the cache

## This function creates a special 'matrix', which is really a list containing
## a function to 
## 1) set the value of the matrix
## 2) get teh value of the matrix
## 3) set the value of the inverse
## 4) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve)  m <<- solve(x)
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}

## This function calculates the inverse of the special 'matrix' created with the 
## above function. It checks to see if the inverse has already been calculated.
## If it has, get the inverse from the cache and skip the computation. Otherwise,
## calculate the inverse of the special 'matrix' and set the value of the 
## inverse in the cache vis the setinverse function.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        return (m)
    }
    data <- x$get()
    m <- solve(data,...)
    x$setinverse(m)
    m
        ## Return a matrix that is the inverse of 'x'
}

