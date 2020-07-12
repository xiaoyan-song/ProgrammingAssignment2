## This function creates a special "matrix" object that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
    inver <- NULL
    ## set the values of matrix
    set <- function(y) {
        x <<- y
        inver <<- NULL
    }
    ## get the value of matrix
    get <- function() x
    ## set the value of the inverse
    setinverse <- function(solve) inver <<- solve
    ## get the value of the inver
    getinverse <- function() inver
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    inver <- x$getinverse()
    if(!is.null(inver)) {
        message("getting cached data")
        return(inver)
    }
    data <- x$get()
    inver <- solve(data, ...)
    x$setinver(inver)
    inver
    ## Return a matrix that is the inverse of 'x'
}
