## These functions makes use of lexical scoping in R to access already computed
## inverse of a matrix available in the cache memory.  This is useful if the inverse
## is accessed repeatedly like in a loop ect.   

## This function returns a list of functions to set, get, setinverse, and getinverse of a matrix x. 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## This function checks to see if the inverse of object x has been calculated 
## and if so accesses from cache, otherwise it completes the calculation 
## and stores the reference to the inverse for later use.   


cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("Using cashed data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
        ## Return a matrix that is the inverse of 'x'
}

