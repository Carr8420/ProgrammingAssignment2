## Friday May 27 2016 Prof. Peng's R Programming Week 3 assignment due Sunday May 29
##  Code is to cache inverse matrix
##  Following function, called makeCacheMatrix, 
##  creates a special "matrix" object that can cache its inverse. 
##  <<- causes a search to be made through parent environments
##  for an existing definition of the variable being assigned.
##  If such a variable is found
##  then its value is redefined,
##  otherwise assignment takes place in the global environment.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

##  Following function, called cacheSolve, computes the inverse
##  of the object returned by make CacheMatrix above.  
##  If the inverse has already been calculated
##  and the matrix has not changed
##  the cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}