## This R file contains two functions which help in caching the inverse of a 
## given matrix and return the inverse matrix from the cache rather than 
## computing the inverse again which will be a costly operation.

## This function returns a special vector that has the matrix and place holder 
## for its inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    ## Function to set the input matrix and the inverse to NULL, so whenever the
    ## matrix is updated using set function, the inverse can be recalculated.
    set <- function(y) {
        x <<- y
        i <<- NULL
    }

    ## Function to get the input matrix
    get <- function() x
    
    ## Function to set the inverse matrix to i
    setinverse <- function(inverse) i <<- inverse
    
    ## Function to get the inverse matrix 
    getinverse <- function() i
    
    ## The list vector containing functions to set and get input matrix and 
    ## functions to set and get the inverse matrix. This special list vector is 
    ## returned from the function
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
