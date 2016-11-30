## The pair of functions below were created for the Programming Assignment 2:
#Lexical Scoping, of the R Programming course (coursera).
#They cache the inverse of a matrix, wich could take some time-consuming
#computations if needed to be calculated over and over again.

##Observation:
#Computing the inverse of a square matrix can be done with 
#the solve function in R.
#For example, if X is a square invertible matrix, then solve(X)
#returns its inverse.

#The functions:

## makeCacheMatrix: This function creates a special "matrix" 
#object that can cache its inverse.

#set value of matrix
#get value of matrix
#set value of inverse matrix
#get value of inverse matrix



makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}

#cacheSolve: This function computes the inverse of the special "matrix"
#returned by makeCacheMatrix above. If the inverse has already been calculated
#(and the matrix has not changed), then the cachesolve should retrieve the
#inverse from the cache.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}


#testing
x <- makeCacheMatrix(matrix(c(1, 1, 1, 3, 4, 3, 3, 3, 4), 3, 3))
x$get()
cacheSolve(x)
cacheSolve(x)
#it seems to be working correctly


###
#the example function and some tests that I ran with it were my basis
#to write the assignment one:

makeVector <- function(x = numeric()) {
        
        m <- NULL
        set <- function(y) {
                
                x <<- y
                m <<- NULL
                
        }
        
        get <- function() x
        
        setmean <- function(mean) m <<- mean
        
        getmean <- function() m
        
        list(set = set, get = get,
             
             setmean = setmean,
             
             getmean = getmean)
        
}

cachemean <- function(x, ...) {
        
        m <- x$getmean()
        
        if(!is.null(m)) {
                
                message("getting cached data")
                
                return(m)
                
        }
        
        data <- x$get()
        
        m <- mean(data, ...)
        
        x$setmean(m)
        
        m
        
}

w <- makeVector(1:4)
cachemean(w)
cachemean(w)

#Thanks!
