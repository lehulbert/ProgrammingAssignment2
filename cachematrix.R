# Together, these two functions save time when trying to find the inverse of
# a matrix. When the cacheSolve function finds the inverse of a matrix, 
# that result gets stored. So, when the user needs to find the inverse of that
# matrix again, the function finds the stored result instead of having to
# perform the inverse function again. 


# The makeCacheMatrix function takes a matrix as an input and 
# establishes a number of functions that can be applied to that matrix. 

makeCacheMatrix <- function(x = matrix()) {
    inv<- NULL
    set <- function(y){
        x<<-y
        inv<<- NULL
    }
    get <-function() x
    setinverse <-function(inverse) inv<<- inverse
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# The cacheSolve function takes a matrix as an input and determines
# if that matrix has been solved before. If it has, then the function returns  
# the previous solution and tells the user that that solution was cached. If the 
# matrix has never been solved before, the cacheSolve function will return the
# inverse of the matrix and pass that result back to the makeCacheMatrix
# function. 

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv))  {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}


