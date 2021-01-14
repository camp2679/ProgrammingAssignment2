##------------------------------------------------------------------------##
## In tandem, the makeCacheMatrix and cacheSolve functions are used to    ##
## solve and cache the inverse of a defined matrix. This saves both user  ##
## and elapsed time by storing the inverse in an object that can be       ##
## retrieved as opposed to continuously recalculating the matrix.         ##     
##                                                                        ##
## Example (w/ output):                                                   ##
## 1. ex <- matrix(c(2,1,1,2), nrow = 2, ncol = 2)                        ##
##                                                                        ##
## 2. myvec <- makeCacheMatrix(ex)                                        ##
##                                                                        ##
## 3. myvec$get()                                                         ##
##      [,1] [,2]                                                         ##
## [1,]    2    1                                                         ##
## [2,]    1    2                                                         ##
##                                                                        ##
## 4. myvec$getsolve()                                                    ##
## NULL                                                                   ##
##                                                                        ##
## 5. cacheSolve(myvec)                                                   ##
##          [,1]       [,2]                                               ##
##[1,]  0.6666667 -0.3333333                                              ##
##[2,] -0.3333333  0.6666667                                              ##
##                                                                        ##
## 6. myvec$getsolve()                                                    ##
##           [,1]       [,2]                                              ##
##[1,]  0.6666667 -0.3333333                                              ##
##[2,] -0.3333333  0.6666667                                              ##
##------------------------------------------------------------------------##

##------------------------------------------------------------------------##
## makeCacheMatrix creates a special "vector" containing 4 functions:     ##
## 1. set the value of the vector                                         ##
## 2. get the value of the vector                                         ##
## 3. set the value of the mean                                           ##
## 4. get the value of the mean                                           ##
## These functions are used in conjunction with the cacheSolve function   ##
## defined in the next section                                            ##
##------------------------------------------------------------------------##

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<-y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m 
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

##------------------------------------------------------------------------##
## cacheSolve calculates and caches the inverse of the matrix provided to ##
## the makeCacheMatrix function above.                                    ##
##------------------------------------------------------------------------##

cacheSolve <- function(x, ...) {
       m <- x$getsolve()

       if(!is.null(m)) {
         message("Getting cached data")
         return(m)
       }
       data <- x$get()
       m <- solve(data, ...)
       x$setsolve(m)
       m
}