## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(invMatrix) inv <<- invMatrix
    getinv<- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached inverse")
        return(i)
    }
    mat <- x$get()
    i <- solve(mat)
    x$setinv(i)
    i
}
print(cacheSolve(makeCacheMatrix(rbind(c(1, -1/4), c(-1/4, 1)))))