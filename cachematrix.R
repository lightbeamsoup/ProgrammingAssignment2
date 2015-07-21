## A series of functions to enable caching of a matrix inverse
## to speed up usage of a frequently utilized capability
## Usage: m <- matrix(...)
## mc <- makeCacheMatrix(m)
## inv <- cacheSolve(mc)

## Function that makes a list of functions to 
## calculate the inverse of a matrix
## Note: run this before cacheSolve!

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL 
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}


## Checks to see if the inverse has already been solved
## If not, calculates it
## Note: this should be run on the list that results from
## running makeCacheMatrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)){
        message("getting cached inverse")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
