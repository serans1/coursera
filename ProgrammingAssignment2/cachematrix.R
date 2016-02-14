## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#Create a cached matrix object that allows basic caching operations
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL #init the inverse to null
    set <- function(y) { #a setter & init... 
        x <<- y
        inv <<- NULL
    }
    get <- function() x # a getter 
    setinv <- function(invMatrix) inv <<- invMatrix #the caching operation 
    getinv<- function() inv #get the cached result
	#and now returned the matrix cache object as a list
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function
#solves the inverse of matrix, caches the results for performance 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinv() # try to get the cached value 
    if(!is.null(i)) { #cahced value found, return it ... 
        message("getting cached inverse")
        return(i)
    }
	#need to caclulate the inverse so get the matrix and solve it 
    mat <- x$get()
    i <- solve(mat)
    x$setinv(i) #cache fill
    i #return the inverse 
}
print(cacheSolve(makeCacheMatrix(rbind(c(1, -1/4), c(-1/4, 1)))))