## The following fuctions calculate and retur the inverse of a matrix from the 
## cache. They reduce the time cost of elaborated computatations
## specially when they have to be conducted repeatedly (e.g. in a loop). 

## This function creates a matrix object that it's able to be 
## retreived from the cache for inversion. 


makeCacheMatrix <- function(x = matrix()) {
        ma <- NULL
        set <- function(y) {
                x <<- y
                ma <<- NULL
        }
        get <- function() x
        setinv <- function(inverseMatr) ma <<- inverseMatr
        getinv <- function() ma
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## If a matrix is already been returden by makeCacheMarix() 
## the following fuction returns the inverse of the 
## calculated matrix from the cache.


cacheSolve <- function(x, ...) {
        ma <- x$getinv()
        if(!is.null(ma)) {
                message("getting cache data")
                return("ma")
        }
        data <- x$get()
        ma <- solve(data)
        x$setinv(ma)
        ma    ## Return a matrix that is the inverse of 'x'
}
