## This R file contains two functions which help in caching the inverse of a 
## given matrix and return the inverse matrix from the cache rather than 
## computing the inverse again which will be a costly operation.

## This function returns a special vector that has the matrix and place holder 
## for its inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function returns the inverse matrix corresponding to the matrix in the 
## given vector x that is returned by makeCacheMatrix function

cacheSolve <- function(x, ...) {
    ## Check if the inverse is already calculated for the matrix
    i <- x$getinverse()
    if(!is.null(i)) {
        ## If the inverse is already calculated, return the calculated value 
        ## from cached data
        message("getting cached data")
        return(i)
    }
    ## If the inverse is not calculated already, calculate using solve function.
    given_matrix <- x$get()
    i <- solve(given_matrix, ...)
    ## Set the calculated inverse in the cache vector
    x$setinverse(i)
    i
}
