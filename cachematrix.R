## The makeCacheMatrix function creates a special matrix object 
## that stores in a variable the inverse of the given matrix
## The cacheSolve function either creates and stores the inverse of the 
## matrix stored in the makeCacheMatrix object, or it simply returns the 
## already stored inverse.

## makeCacheMatrix returns a list of four functions responsible for setting (set)
## and returning (get) a given matrix, and also setting (setinverse) and returning
## (getinverse) the inverse of the matrix, which is stored in an inner variable (inv)

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## cacheSolve returns the inner stored variable inv of the given function-object 
## of makeCacheMatrix type (x). If the inv variable does not hold the inverse of 
## the matrix of makeCacheMatrix, it is calculated and stored in the makeCacheMatrix
## using the setinverse function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getinverse()
    
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    data <- x$get()
    
    inv <- solve(data, ...)
    
    x$setinverse(inv)
    inv
    
}
