## We have two functions here makeCacheMatrix and cacheSolve
## These functions calculate the inverse of a matrix and store it in the cache
## If the value of the inverse is present in the cache, the function returns
## the cached value, 
## else calculates the value of the inverse and stores it in the cache



## makeCacheMatrix creates a special matrix which will cache the inverse value 
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) {
        i <<- inverse
    }
    getInverse <- function() i
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## This function checks if the value of the inverse is present in the cache, 
## If the value returned by the cache is not null, it is a cache hit and return
## the value retrieved from the cache
## Else, it is a cache miss and calulate the inverse and return it
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
    i<-x$getInverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    message("Cache miss. Calculating inverse of the matrix")
    input <- x$get()
    i <- solve(input,...)
    x$setInverse(i)
    i
}
