## These functions take a square maxtrix and
## returns the inverse matrix

## This function returns creates a matrix and contains
## the set, get, setmatrix and getinverse functions

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        
        setmatrix <- function(solve) m <<- solve
        
        getinverse <- function() m
        
        list(set = set, get = get,
             setmatrix = setmatrix,
             getinverse = getinverse)
}


## This function returns cached inverse matrix
## or solves inverse matrix then caches it

cacheSolve <- function(x, ...) {
        
        m<-x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        matrix<-x$get() 
        m<-solve(matrix, ...)
        x$setmatrix(m)
        m
}
