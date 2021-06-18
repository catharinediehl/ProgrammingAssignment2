## The two functions, makeVector and makeCache, cache the inverse of a matrix.
## This allows us to calculate the inverse of the matrix just once, save it in 
## the cache. Then, when the inverse is needed again, the value is retrieved 
## from the cache, rather than re-calculating it. 

## This function creates a list with values to be used by the cacheSolve function.
## It takes as input a matrix, x, and outputs a list with four functions, 
## set, get, setinverse, and getinverse, as well as two objects, x and m. 
## These will save the inverse of the matrix and provide cache solve with the
##inverse. 
 

makeVector <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m 
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## This function takes an argument, x, of the form "makeVector(a)"
##where a is a square matrix. It will then use the value of the inverse
##of a that is cached using the makeVector function. 



cacheSolve <- function(x, ...) {
        
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