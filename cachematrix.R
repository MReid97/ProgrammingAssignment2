## These functions will cache the inverse of a matrix which is an efficient alternative to repeatedly computing things.

## This function will create the cache for the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
        inv<- NULL
        set<- function(y){
                x<<- y
                inv<<- NULL
        }
        get <- function() x
        
        setinverse <- function(inverse) inv <<- inverse 
        getinverse <- function() inv  
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This is the function that will actually compute the inverse of the newly created matrix

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("retrieving cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
        
