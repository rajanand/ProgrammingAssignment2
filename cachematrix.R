## The below functions calculate the inverse of a matrix and saves it
## to the cache so that when next time the user attempts to calculate the matrix inverse, 
## the previously saved value is returned instead of doing the calculation again.

## This function creates a special matrix object which is a list containing a function to

## a. set the value of the matrix
## b. get the value of the matrix
## c. set the value of the inverse
## d. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        
        ## define the cache 'm'
        m <- NULL
        set <- function(y) {
                x <<- y 
                m <<- NULL ## re-initialize m in the parent environment to NULL
        }
        get <- function() x  
        setinverse <- function(inverse) m <<- inverse 

        getinverse <- function() m ## return the cached inverse of x
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The following function calculates the inverse of the special matrix created with the above function. 
##Though, it first checks to see if the inverse has already been caclulated. If yes, then it get's the inverse from the cache
## and skips the computation part. Otherwise, it calculates the matrix inverse and sets the value of the inverse 
##in the cache.

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
