## Put comments here that give an overall description of what your
## functions do

## Creates a matrix that can hold the inverse of the given matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(val) inv <<- val
    getinv <- function() inv
    list(set = set, get = get, getinv = getinv, setinv = setinv)
}


## This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if (!is.null(inv)){
        message("getting the cached value")
        return(inv)
    }
    new_matrix <- x$get()
    inv <- solve(new_matrix)
    x$setinv(inv)
    inv
}
