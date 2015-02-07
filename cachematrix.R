#CACHING THE INVERSE OF A MATRIX

#Author: Daniel Alfonso Silva Barrera
#Date: 02.07.2015

# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix: Creates a special "matrix", which is really a list containing 
# a function to:
# 1. Set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    
    #Set the value of the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # Get the value of the matrix
    get <- function() x
    
    #Set the value of inverse of the matrix
    setinverse <- function(inverse) inv <<- inverse
    
    # Get the value of inverse of the matrix
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# cacheSolve: calculates the mean of the special "matrix" created with the above 
# function. However, it first checks to see if the mean has already been calculated.
# If so, it gets the mean from the cache and skips the computation. Otherwise, 
# it calculates the mean of the data and sets the value of the mean in the cache via
# the setmean function.
#This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
    
    inv <- x$getinverse()
    
    #Later calls
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    #First call
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}

# HOW TO USE:

# Create matrix
# > matrix = rbind(c(1, -1/4), c(-1/4, 1))
# > m_cache = makeCacheMatrix(matrix)

# View data
# > m_cache$get()
#       [,1]  [,2]
# [1,]  1.00 -0.25
# [2,] -0.25  1.00

# First call (no cache)
# > cacheSolve(m_cache)
#           [,1]      [,2]
# [1,] 1.0666667 0.2666667
# [2,] 0.2666667 1.0666667

# Later calls (getting cached data)
# > cacheSolve(m_cache)
# getting cached data.
#           [,1]      [,2]
# [1,] 1.0666667 0.2666667
# [2,] 0.2666667 1.0666667
