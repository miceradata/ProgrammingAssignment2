## ProgrammingAssignment2
## Example:
##
## > m <- matrix(c(1,1,1,3,4,3,3,3,4), 3, 3)
## > m
##      [,1] [,2] [,3]
## [1,]    1    3    3
## [2,]    1    4    3
## [3,]    1    3    4
## > solve(m)
##       [,1] [,2] [,3]
## [1,]    7   -3   -3
## [2,]   -1    1    0
## [3,]   -1    0    1
## 
## > c <- makeCacheMatrix(c(1,1,1,3,4,3,3,3,4), 3, 3)
## > cacheSolve(c)
##       [,1] [,2] [,3]
## [1,]    7   -3   -3
## [2,]   -1    1    0
## [3,]   -1    0    1
## 
## 
## Creates an object with access functions get and set,
## and functions to store the inverse matrix,
## setinverse and getinverse.
## Calculating the inverse must be done using cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) m <<- inv
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Returns a matrix that is the inverse of 'x',
## where x is an object returned by makeCacheMatrix.
## If the inverse has already be calculated,
## the result is returned immediately.
## Otherwise, the inverse is first calculated using 'solve',
## stored in 'x' and returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
