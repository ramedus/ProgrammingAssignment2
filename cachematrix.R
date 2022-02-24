## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The makeCacheMatrix function creates a matrix that can 
## cache its inverse. The code is shown below.

makeCacheMatrix <- function(x = matrix()) {
                INVERSA <- NULL
                          set <- function(y) {
                            x <<- y
                            INVERSA <<- NULL
                          }
                          get <- function() x
                          setIv <- function(inverse) INVERSA <<- inverse
                          getIv <- function() INVERSA
                          list(set=set, 
                               get=get, 
                               setIv=setIv, 
                               getIv=getIv)
}


## Write a short comment describing this function

## With the cacheSolve function, the inverse matrix created previously with 
## the makeCacheMatrix function above is calculated. If the inverse has 
## already been computed (and the matrix hasn't changed), then you should 
## retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
          kk <- x$getIv()
          if (!is.null(kk)) {
            message("getting cached data")
            return(kk)
          }
          data <- x$get()
          kk <- solve(data, ...)
          x$setIv(kk)
          kk
}


##EXAMPLE:

    ## Sample run:
    ## > x = rbind(c(1, -5/10), c(-5/10, 1))
    ## > m = makeCacheMatrix(x)
    ## > m$get()
    ##       [,1]  [,2]
    ## [1,]  1.00 -0.5
    ## [2,] -0.5  1.00

    ## No cache in the first run
    ## > cacheSolve(m)
    ##           [,1]      [,2]
    ## [1,] 1.3333333 0.6666667
    ## [2,] 0.6666667 1.3333333


    ## > cacheSolve(m)
    ## getting cached data.
    ##           [,1]      [,2]
    ## [1,] 1.3333333 0.6666667
    ## [2,] 0.6666667 1.3333333


## thanks
