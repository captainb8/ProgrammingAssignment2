## The functions below compute and cache the inverse of a square matrix.  They can return 
## either the original matrix or the inverse matrix.  If inverse matrix has been previously
## cached, it will be returned, rather than computing the inverse each time.


## This function accepts a matrix as an argument and returns a list made up of functions which 
## get and set the original matrix and the inverse matrix
## The "setinverse" function within caches the invserse matrix
makeCacheMatrix <- function(x = matrix()) {
    invmatrix <- NULL
    set <- function(y) {
        x <<- y
        invmatrix <<- NULL
    }
    get <- function(){ x }
    setinverse <- function(inversed) invmatrix <<- inversed
    getinverse <- function() invmatrix
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function checks to see if the inverse matrix already exists (previously cached).
## If not, it computes the inverse of the matrix provided and caches it via the makeCacheMatrix function
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()               #get the cached inverse matrix (could be null)
    if(!is.null(inverse)) {                 #check to see if it's null
        message("getting cached data")
        return(inverse)                     #if not null, return it, avoiding the computation
    }
    data <- x$get()
    inverse <- solve(data, ...)     #compute the inverse
    x$setinverse(inverse)           #cache the inverse by passing it to the makeCacheMatrix funciton
    inverse
}