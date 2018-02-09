## the makeCacheMatrix function and the cacheSolve function are used together to   
## cache the inverse of a matrix.

## The makeCacheMatrix function uses an invertible matrix as its argument and returns a 
## list that has the following:
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse matrix
## 4.  get the value of the inverse matrix



makeCacheMatrix <- function(x = matrix()) {
    m_inverse <-NULL
    set <- function(y) {
        x <<- y
        m_inverse<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m_inverse <<- solve
    getinverse <- function() m_inverse
    
    list(set=set,get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The cacheSolve function calculates the inverse of a matrix using the result of the 
## makeCacheMatrix as its argument.  First, it checks to see if the
## inverse matrix has already been calculated. If so, it `get`s the inverse matrix from the
## cache and skips the computation. If the inverse isn't calculated, it calculates the inverse
## of the data and sets the value of the inverse matric in the cache via the `setinverse`
## function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m_inverse <- x$getinverse()
    if(!is.null(m_inverse)) {
        message("getting cached data")
        return(m_inverse)
    }
    data <- x$get()
    m_inverse <- solve(data, ...)
    x$setinverse(m_inverse)
    m_inverse
}

