##
## cachematrix.R
##
## constructor and helper for initializing and caching the inverse
## of invertible matrices.
##

##
## makeCacheMatrix
##
## constructor for "cache matrix" objects.
## these objects simply cache the inverse of the given matrix x,
## so that you don't have to calculate that inverse over and over
## at runtime.  this function assumes that x is incertible, and so
## no checking is performed to that end.
##
## note that the initial value of the inverse of x has to be calculated
## with the helper function cacheSolve.
##
## for example:
## x <- someCoolMatrix
## cacheOfXInverse <- makeCacheMatrix(x)
## cacheSolve(cacheOfXInverse)
##
## now you can get the inverse of x:
## cacheOfXInverse$getinverse()
##
makeCacheMatrix <- function(x = matrix()) {
    cachedinverse <- NULL

    getinverse <- function() cachedinverse
    setinverse <- function(newinverse) cachedinverse <<- newinverse
    get <- function() x
    set <- function(newmatrix){
        x <<- newmatrix
        cachedinverse <<- NULL
    }

    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


##
## cacheSolve
##
## helper function that calculates and stores the inverse of the given
## "cache matrix" object.  use the constructor function makeCacheMatrix
## to construct such an object.
##
## note that this function assumes the matrix for oCacheMatrix is invertible,
## and will throw runtime errors if that is not the case.
##
cacheSolve <- function(oCacheMatrix, ...) {
    ## Return a matrix that is the inverse of 'oCacheMatrix'
    inverse <- oCacheMatrix$getinverse()
    if(!is.null(inverse)) {
        message("getting cached inverse")
        return(inverse)
    }

    data <- oCacheMatrix$get()
    inverse <- solve(data,...)
    oCacheMatrix$setinverse(inverse)
    inverse
}
