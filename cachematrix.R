## The following R functions are written to cache the potentially time consuming activity of 
## calcuting the inverse of the matrix. The inverse is first checked in the cache and if present returned.
## If not, it is calcuated.

## The following function - makeCacheMatrix, creates a vector, which is a list of functions to do 
## the following:
## 1) Set the value of the matrix 
## 2) Get the value of the matrix
## 3) Set the value of the inverse of the matrix
## 4) Get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## The following function - cacheSolve, calculates the inverse of a matrix using the makeCacheMatrix function.
## However, it first checks to see if the inverse already exist in the cache, if so it gets the
## inverse from cache using "getInverse". Otherwise, it calculates the inverse of the matrix and
## sets the inverse of the matrix in the cache using setInverse function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat,...)
    x$setInverse(inv)
    inv
}
