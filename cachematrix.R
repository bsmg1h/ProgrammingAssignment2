######## Comment #########

## This pair of functions can realize caching the result of matrix inversion,   
## in order to prevent repeating calculating the inversion that has been done 
## before. This can be useful especially when the matrix is very big, caching 
## the result means saving so much times.

## I completed these two functions with the help from Mr.Peng, imitating his two 
## functions which can cache the mean of a vector. After that i do a few tries 
## to test the properties of the function pair.


######## The first function ########

## The first function can preprocess the input matrix to a list of 4 function 
## c("set","get","setsolve","getsolve")
## There are mainly two goals.
##    1. To cache the input matrix in the closure and pass it to the next 
##    function to calculate
##    2. To cache the output inversion result in the closure.
## The first goal is realized by functions"set"and"get". The second goal is 
## realized by functions"setsolve" and "getsolve"

makeCacheMatrix <- function(x = matrix()) {
    # reset the inversion result
    sol <- NULL
    
    # realize the first goal
    set <- function(y) {
      x <<- y
      sol <<- NULL # reset the inversion result in the closure
    }
    get <- function() x
    
    # realize the second goal
    setsolve <- function(solve) sol <<- solve
    getsolve <- function() sol
    
    # out put these 4 functions as a list
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## We can find that whether we use makeCacheMatrix(x) or $set(x) function to 
## reset the matrix, the "sol" value will be reset. 


######## The Second function ########

## There are also 2 main goals with the Second function.
##    1. To recieve the matrix and the inversion result from the first function.
##    2. To test if the inversion result exist. If not, calculate the inversion.
##    Otherwise, directly output the inversion result and a message noting that
##    the result comes from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    sol <- x$getsolve()
    if(!is.null(sol)) {
      message("getting cached data")
      return(sol)
    }
    data <- x$get()
    sol <- solve(data, ...)
    x$setsolve(solve)
    sol
}

## What we should notice that the parameter "x" in the function "cacheSolve" 
## is not a matrix. It has to be the result that handled by the function 
## "makeCacheMatrix" first, a list that containing 4 functions.



######### A few experiments #########

## 1. Firstly, we do a experiment to test whether it can save time

A <- matrix(data = rnorm(1000000), ncol = 1000, nrow=1000)

## the time used to directly solve the matrix
start.time <- Sys.time()
B1 <- solve(A)
end.time <- Sys.time()
time.use <- end.time - start.time
time.use
# Time difference of 0.9555659 secs


## the time used to solve the matrix using the two functions I built
B <- makeCacheMatrix(A)

start.time <- Sys.time()
C <- cacheSolve(B)
end.time <- Sys.time()
time.use <- end.time - start.time
time.use
# Time difference of 0.9605551 secs

start.time <- Sys.time()
D <- cacheSolve(B)
# getting cached data
end.time <- Sys.time()
time.use <- end.time - start.time
time.use
# Time difference of 0.001000881 secs

## We can see that though it cost a bit more time comparing to the directly 
## solving ,if the result comes from the cache, the time is saved by 99.9%


## 2. Then we test the condition when the inversion can come from the cache
rm(A,B,B1,C,D)

A <- matrix(data = rnorm(1000000), ncol = 1000, nrow=1000)
B <- makeCacheMatrix(A)
C <- cacheSolve(B)
D <- cacheSolve(B)
# getting cached data
## the result of repeating computing comes from the cache

rm(A,B,C,D)

A <- matrix(data = rnorm(1000000), ncol = 1000, nrow=1000)
B <- makeCacheMatrix(A)
C <- cacheSolve(B)
A <- matrix(data = rnorm(1000000), ncol = 1000, nrow=1000)
B <- makeCacheMatrix(A)
D <- cacheSolve(B)
## reset a different input matrix, then the result will not exist in the cache

rm(A,B,C,D)

A <- matrix(data = rnorm(1000000), ncol = 1000, nrow=1000)
B <- makeCacheMatrix(A)
C <- cacheSolve(B)
B$set(A)
D <- cacheSolve(B)
## reset a different input matrix by using the function"set",even though the 
## matrix is not changed actually, the result is not exist in the cache still.

## It indicates that this pair of functions can not detect whether the matrix 
## is really changed. As long as the matrix is reset, the result has to be 
## recomputed. Maybe in the future, we can attempt to add a detecting module to 
## the function so that the result will be recomputed only when the matrix is 
## really changed.


