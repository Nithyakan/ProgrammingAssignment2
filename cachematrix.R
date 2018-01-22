## Put comments here that give an overall description of what your
## functions do

# This script contains two functions that will be used to calculate inverse of a square
# matrix which are assumed to be invertible using the cached value of the inverse matrix 
# instead of repeatedly calculating the inverse of the same matrix again and again.

## Write a short comment describing this function

# makeCacheMatrix function takes a square matrix as input and creates a data frame 
# (special matrix object) object that contains four functions namely set, get, setinverse, getinverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinversematrix <- function(inversematrix) inv <<- inversematrix
    getinversematrix <- function() inv
    data.frame(matrix(c(set, get, setinversematrix,getinversematrix),nrow=1,ncol=4,dimnames = list(c(),  c("set", "get", "setinversematrix","getinversematrix"))))
}


## Write a short comment describing this function
# cacheSolve function takes the data frame object created using the above makeCacheMatrix function as input.
# Then returns the inverse of the matrix if the inverse has already been calculated (and the matrix has not changed), 
# otherwise cacheSolve function computes the inverse matrix and returns the inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinversematrix[[1]]()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get[[1]]()
    inv <- solve(data, ...)
    x$setinversematrix[[1]](inv)
    inv
}
