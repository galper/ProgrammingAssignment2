## There are 2 functions:

## (1)makeCacheMatrix: This function creates a special 
##     "matrix"(in fact list) object that can cache its inverse.
##     It contains 4 special functions:
##             set matrix
##             get matrix
##             set inverse matrix
##             get inverse matrix

##  (2)cacheSolve: This function computes the inverse 
##     of the special "matrix" returned by makeCacheMatrix above. 
##     If the inverse has already been calculated (and the matrix has not changed), 
##     then the cacheSolve should retrieve the inverse
##     from the cache with message .



## Creating special "matrix"

makeCacheMatrix <- function(x = matrix()) {
        # if a matrix is called without a method
        m<- NULL
        set <- function(y){
                x<<-y
                m<<-NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
}




## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        
        m <- x$getinv()
        if(!is.null(m)) 
        {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
