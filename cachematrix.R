## The idea behind this assignment is to cache the inverse of a matrix
## and thus save the time necessary to compute it over and over again.
## The first function, makeCacheMatrix, sets the values of the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
## 1) now it sets the value of the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
## 2) next it gets the value of the matrix  
    get <- function() x
## 3) then it sets the value of inverse of the matrix    
    setinverse <- function(inverse) inv <<- inverse
## 4) and finally it gets the value of inverse of the matrix  
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## The second function, cacheSolve, gives us the inverse of the matrix. 
## First, the function checks to see if the inverse has already been calculated, 
## and if this is the case, the function presents the result without doing any
## calculation. 

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
## If the inverse has not been calculated, the function does this calculation and 
## then sets the inverted matrix's value in the cache
## through the setinverse function (setinv) and then returns the inverted matrix.
      matrix.data = x$get()
        inv = solve(matrix.data, ...)
        
        x$setinv(inv)
        
        return(inv)
}
