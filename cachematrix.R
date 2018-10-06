## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function defines x as a matrix and follows the getter-setter approach to create a a list in the end that contains the follwoing elements:
# First it sets i to NULL thereby creating the object i and reserves memory to it. The following st function contains the argument y and only set y to the x element of the global environment.
# and again NULL to the object i. Therefore whenever there is an inverse stored and x is reset the i value is cleared and set to NULL so that a new inverse value can be stored in i.
# The following get just calls the object x.
# setinv: Since i is set in the parent environment and we need to access it after setinv() is executed we assign the <<- again just as in the set function.
# getinv: takes the object i agin from the parent environment.
# Finally list creates a list of all the created objects so that they can get called.

# Please execute the below code and scroll to the bottom for an example


makeCacheMatrix <- function(x = matrix()) {
        #makeVector <- function(x = numeric()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}
## Write a short comment describing this function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached inv data")
                return(i)
        }
        data <- x$get()
        i <- solve(data,...)
        x$setinv(i)
        i
}
        
# Explanation of single items in CacheMatrix and Solve Matrix 
#        A <- matrix(c(1,2,3,4),2,2)    # define matrix
#        aMatrix <- makeCacheMatrix(A)  # assign makeCacheMatrix to aMatrix
#        aMatrix$get()                  # retrieve the value of x
#        aMatrix$getinv()               # retrieve the value of m, which should be NULL
#        aMatrix$set(y)                 # reset value with a new matrix
#        cacheSolve(aMatrix)            # inv calculated
#        aMatrix$getinv()               # retrieve it directly, now that it has been cached


# Example of Use below 
B <- matrix(c(1,2,3,4),2,2) 
B1 <- makeCacheMatrix(B)
cacheSolve(B1) 
        
#To see that the cached data is retrived run the last command again
cacheSolve(B1)         
