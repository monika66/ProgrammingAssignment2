##
## File       cachematrix.R
##
## Coursera   Data Science Specialization
## course 2   R programming, 
## week 3     peer-graded Assignment
## student    Monika Hunkeler
## date       30.4.2017
##
## Programming Assignment 2: Lexical Scoping - Caching the Inverse of a Matrix
##
## The function 'makeCacheMatrix' of the file 'cachematrix.R' creates a special 
## "matrix" object that can cache its inverse. If the inverse has already been 
## calculated and the matrix has not changed, then the function 'cacheSolve' 
## should retrieve the inverse from the cache.

## The function 'makeCacheMatrix' caches a matrix x in the memory and returns
## a list of methods to get and set data from matrix x and the invers of x 

makeCacheMatrix <- function(x = matrix()) {
        minv <- NULL
        set <- function(y) {
                # assign data from the parent environment
                x <<- y
                minv <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) minv <<- solve
        getsolve <- function() minv
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## The function 'cacheSolve' returns a matrix that is the inverse of matrix x

cacheSolve <- function(x, ...) {
        # if the inverse of the same matrix was calculated before 
        # return 'minv' with the cached values from the memory 
        minv <- x$getsolve()
        if(!is.null(minv)) {
                message("getting cached data")
                return(minv)
        }
        # else get the matrix x and calculate the invers of x
        # cache invers x in the memory and assign it to 'minv'
        data <- x$get()
        minv <- solve(data, ...)
        x$setsolve(minv)
        minv
}
