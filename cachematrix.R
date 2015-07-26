## pair of functions that cache the inverse of a matrix.


## creates a special “matrix” object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
 
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) i <<- inverse
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


# computes the inverse of the special "matrix" returned by makeCacheMatrix function
cacheSolve <- function(x = matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        
		if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
		matrix <- x$get()
        i <- solve(matrix, ...)
        x$setinv(i)
        i
		
}
