#makeCacheMatrix() creates an R object that stores a matrix and its inverse.

makeCacheMatrix <- function(x=matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(res) inv <<- res
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}

#cacheSolve() requires an argument that is returned by makeCacheMatrix() in order to retrieve the 
#iverse matrix from the cached value that is stored in the makeCacheMatrix() object's environment.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
