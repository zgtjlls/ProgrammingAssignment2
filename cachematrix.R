## Put comments here that give an overall description of what your
## functions do
## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than computing it repeatedly
## Below are two functions: makeCacheMatrix makes a special matrix object
## and cacheSolve either returns the already computed inverse or computes its
## inverse and returns it.

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    set_inv <- function(inv_new){
        inv <<- inv_new
    }
    get_inv <- function () inv
    list(set = set, get = get,
         set_inv = set_inv,
         get_inv = get_inv)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated , then 
## cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$get_inv()
    if (!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$set_inv(inv)
    inv
}

## test and example
m1 <- makeCacheMatrix(matrix(c(1,2,3,4), nrow = 2, ncol = 2))
m1$get()
cacheSolve(m1)
m1$get_inv()


m2 <- makeCacheMatrix(matrix(c(1,2,3,4,-2,-8,4,5,10), nrow = 3, ncol = 3))
m2$get()
cacheSolve(m2)
m2$get_inv()
