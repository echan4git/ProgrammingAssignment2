

# The makeCacheMatrix function creates a special "matrix" object that 
# can cache its inverse, which is a list containing four functions to 
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse of the matrix
# 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    
    # initialize the inverse of the matrix to NULL
    matrixinverse <- NULL
    
    # function 1
    set <- function(y) {
        x <<- y
        matrixinverse <<- NULL
    }  
    
    # function 2 - get the matrix
    get <- function() x
    
    # function 3 - compute the inverse of the matrix and store it in cache
    setinverse <- function(solve) matrixinverse <<- solve
    
    # function 4 - get the inverse of the matrix from cache
    getinverse <- function() matrixinverse
    
    # create a list with the four functions
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# The cacheSolve function computes the inverse of the special "matrix" 
# object returned by the makeCacheMatrix function. If the inverse has 
# already been calculated (and the matrix has not changed), then
# cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    
    matrixinverse <- x$getinverse()
    
    # If inverse from the cache exists, return the cache inverse 
    # and display the message.
    if(!is.null(matrixinverse)) {
        message("getting cached data")
        return(matrixinverse)
    }
    
    data <- x$get()
    
    # calculate the inverse of the matrix
    matrixinverse <- solve(data, ...)
    
    x$setinverse(matrixinverse)
    
    # return the inverse
    matrixinverse
}



# Test the functions
# create a test matrix
# matrix_test <- matrix(c(1,0,5,2,1,6,3,4,0),nrow = 3,ncol = 3)

# matrix_test
#       [,1] [,2] [,3]
# [1,]    1    2    3
# [2,]    0    1    4
# [3,]    5    6    0

# matrix1 <- makeCacheMatrix(matrix_test)

# call cacheSolve first time

# cacheSolve(matrix1)
#       [,1] [,2] [,3]
# [1,]  -24   18    5
# [2,]   20  -15   -4
# [3,]   -5    4    1


# call cacheSolve second time ... get cached data and message is print 

# cacheSolve(matrix1)
# getting cached data
#       [,1] [,2] [,3]
# [1,]  -24   18    5
# [2,]   20  -15   -4
# [3,]   -5    4    1