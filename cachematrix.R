#makeCacheMatrix creates a special matrix object
makeCacheMatrix <- function(x = matrix()) {
        inv_x <- NULL
        
        #the set-function sets values
        set <- function(y) {
                x <<- y
                inv_x <<- NULL
        }
        
        #the get-function returns x (matrix)
        get <- function() x
        
        #create functions for setting and getting the inverse
        setinverse<- function(inverse) inv_x <<-inverse
        getinverse <- function() inv_x
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

# cacheSolve calculates the inverse of the matrix.
cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
        inv_x <- x$getinverse()
        
        #check to see if inv_x not is null, if so then the inverse matrix is cached
        if (!is.null(inv_x)) {
                print("reading solution from cache..")
                return(inv_x)
        } else {
                print("need to calculate this for the first time")
                
                # calculate the inverse matrix
                inv_x <- solve(x$get())
                
                # cache the outcoume from the calculation
                x$setinverse(inv_x)
                
                return(inv_x)
        }
}
