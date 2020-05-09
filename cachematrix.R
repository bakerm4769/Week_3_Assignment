#This function creates a "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
        d <- NULL
        set <- function(y){
                x <<- y
                d <<- NULL
        }
        get <- function()x
        setInverse <- function(inverse) d <<- inverse
        getInverse <- function() d 
        list(set = set, get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}


#Thefunction below computes the inverse of the "matrix" 
#returned by makeCacheMatrix above retriving the inverse from the cache.



cacheSolve <- function(x, ...) {
        
        
        d <- x$getInverse()
        if(!is.null(d)){
                message("getting cached data")
                return(d)
        }
        mat <- x$get()
        d <- solve(mat,...)
        x$setInverse(d)
        d
}