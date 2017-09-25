#############################################################################################
## Program:     cachematrix.R
##              Version: R x64 3.4.1
## Programmer:  caa25
## Date:        September 25, 2017
## Purpose:     R Programming Assignment 2
##              A R program to demonstrate the use of caching time-consuming computations
##              using matrix inversion as an example.  The inverse of a matrix is the matrix
##              which, when multiplied by the original matrix, results in the identity matrix.
##              The identity matrix is a matrix in which all the elements of the principal 
##              diagonal are ones and all other elements are zeros.
##
##              Note: Run test() to test the matrix inversion and cache functions.
##
#############################################################################################

## A function to create a special R matrix object to cache its inverse
## ------------------------------------------------------------------------------------------

makeCacheMatrix <- function(x = matrix()) {
    
    ## Initialize inverse object
    
    inverse <- NULL
    
    ## Define object properties to set and access a cached matrix
    
    set <- function(y) {
        x <<- y
        inverse <<- NULL
        }
    
    get <- function() x
    
    ## Define object properties to set and access a new inverse matrix
    
    setinverse <- function(x) inverse <<- x
    
    getinverse <- function() inverse
    
    ## Create access list of named properties 
    
    list( set = set,
          get = get,
          setinverse = setinverse,
          getinverse = getinverse  )

} ## Close makeCacheMatrix function_________________________________________________________


## A function to invert a matrix returned by makeCacheMatrix().
## Function checks for exising stored inverse prior to performing the inverse calculation.
## If no inverse exists, it inverts the matrix and caches (stores) the inversion.
## NOTES: 
##      1. The x arg is the output of makeCacheMatrix()
##      2. Can only be called on an invertible square matrix.
## ------------------------------------------------------------------------------------------

cacheSolve <- function(x, ...) {
       
    ## Test for existing inverse in cache and display if available
    
    inverse <- x$getinverse()
    
    if (!is.null(inverse)) {
        cat ("Reading cache data...\n")
        inverse
        
        } ## Close if statement
    
    ## Calculate inverse and cache 
    
    else {
        temp <- x$get()
        inverse <- solve(temp,...)
        x$setinverse(inverse)
        
        ## Display inverse matrix
        cat("New inverse matrix:\n")
        inverse
        
        } ## Close else statement

} ## Close cacheSolve function______________________________________________________________


## A function to test the matrix inversion and cache functions
##------------------------------------------------------------------------------------------

test <- function() {
    
    ## Create Test Metrices
    
    test1 <- matrix( c(4,2,7,6), 2, 2)
    cat("Test Matrix 1\n")              ## Should print the following:  4  7
    print(test1)                        ##                              2  6
    cat("\n")

    test2 <- matrix ( c(3,2,0,0,0,1,2,-2,1), 3, 3)
    cat("Test Matrix 2\n") 
    print(test2)                        ## Should print the following:  3  0  2
                                        ##                              2  0 -2
                                        ##                              0  1  1
    cat("\n") 
    
    ## Test Matrix 1
    
    cat ("Testing Matrix 1 as new inversion:\n")
    temp1 <- makeCacheMatrix(test1)
    sol1 <- cacheSolve(temp1) 
    print(sol1)                         ## Note: Inverted matrix should be:  0.6   -0.7
                                        ##                                  -0.2    0.4
    
    cat("\nTesting Maxrix 1 from cache:\n ")
    sol1 <- cacheSolve(temp1)                    
    print(sol1)                         ## Note: Should be the same matrix
    
    cat ("\nTesting for the identity matrix:\n")
    print(round(test1 %*% sol1,0))      ## Should print the identity matrix:  1   0
                                        ##                                    0   1
    
    ## Test Matrix 2
    
    cat ("\n\nTesting Matrix 2 as new inversion:\n")
    temp2 <- makeCacheMatrix(test2)
    sol2 <- cacheSolve(temp2) 
    print(sol2)                         ## Note: Inverted matrix should be:  0.2  0.2  0
                                        ##                                  -0.2  0.3  1                                
                                        ##                                   0.2 -0.3  0
    
    cat("\nTesting Maxrix 2 from cache:\n ")
    sol2 <- cacheSolve(temp2)                    
    print(sol2)                         ## Note: Should be the same matrix
    
    cat ("\nTesting for the identity matrix:\n")
    print(round(test2 %*% sol2,0))      ## Should print the identity matrix:  1   0   0
                                        ##                                    0   1   0
                                        ##                                    0   0   1
    
} ## Close test function__________________________________________________________________
