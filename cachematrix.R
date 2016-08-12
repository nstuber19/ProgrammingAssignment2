## 
## R Programming - Week 3
## Programming Assignment 2
## Nick Stuber

# Write the following functions:
# 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# 2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
# above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
# should retrieve the inverse from the cache.




## makeCacheMatrix function will define 4 functions and place them into a list:
## 1. set function will 
## 2. get function will return matrix x
## 3. set_inverse function will assign the inputed value to variable called inverse_matrix
## 4. get_inverse function will return inverse_matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse_matrix <- NULL
    set <- function(y) {
        x <<- y
        inverse_matrix <<- NULL
    }
    get <- function() x
    set_inverse <- function(inv) inverse_matrix <<- inv
    get_inverse <- function() inverse_matrix
    list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
    
}


## cacheSolve function will calculate the inverse matrix of the input matrix x, but will first
## check to see if the inverse matrix for matrix x has already been calculated/cached. If so,
## it will return the cached value. Else, it will calculate the matrix inverse and cache the 
## calculated inverse matrix. 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse_matrix <- x$get_inverse()
    if(!is.null(inverse_matrix)) {
        message("getting cached data")
        return(inverse_matrix)
    }
    data <- x$get()
    inverse_matrix <- solve(data, ...)
    x$set_inverse(inverse_matrix)
    inverse_matrix
    
}
